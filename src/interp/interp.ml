exception NYI
exception IllTyped
open Sparse.Syntax;;

module Val = struct
  type t =
    | Num of int
    | Bool of bool
    | Fun of Ifer.t * Exp.t
    | Handler of Exp.effectsig list
    | EffInv of Exp.effect
    | Triv

  let to_expr value : Exp.t =
    match value with
    | Num i -> Exp.NumLit i
    | Bool b -> Exp.BoolLit b
    | Fun (var, body) -> Exp.Fun(var, body)
    | Handler elist -> Exp.Handler (elist)
    | Triv -> Exp.Unit
    | EffInv (name, args, ifer, cont) -> EffInv (name, args, ifer, cont)
end



let rec subst (to_subst : Exp.t) (ifer : Ifer.t) (e : Exp.t) =
  (* curry the recursive call for use in multiple cases *)
  let substc = (subst to_subst ifer) in 

  match e with
  | Unit -> e
  | BoolLit _ -> e
  | NumLit _ -> e
  | Var name -> 
     if Ifer.eq name ifer then to_subst else e
  | Prim1 (op, e) -> Prim1 (op, substc e)
  | Prim2 (e1, op, e2) -> Prim2 (substc e1, op, substc e2)
  | If (e1, e2, e3) -> If (substc e1, substc e2, substc e3)
  | Fun (var, body) -> 
     if Ifer.eq var ifer then e else substc body
  | DoIn (var, bounde, body) ->
     DoIn (var,
           substc bounde,
           if Ifer.eq var ifer then body else substc body)
  | With (e1, e2) ->  With(substc e1, substc e2)
  | EffInv (effname, args, ret_name, cont) ->
     EffInv (effname,
          List.map (fun e -> substc e) args,
          ret_name,
          if Ifer.eq ret_name ifer then cont else substc cont)
  | Handler efflist ->
     Handler (List.map
       (fun (effname, varlist, cname, body) ->
         let shad_vars =
           List.fold_left
             (fun acc var -> acc || Ifer.eq var ifer)
             false
             varlist
         in 
         (effname, varlist, cname,
          if shad_vars || Ifer.eq cname ifer then body else substc body))
       efflist)

let get_handler (effects : Exp.effectsig list) (name : Ifer.t) : Exp.effectsig option =
  List.find_opt
    (fun eff->
      let (eff_name,_,_,_) = eff in
      Ifer.eq eff_name name)
    effects
      
                                                                 
let prim2_num : Exp.prim2 -> int -> int -> int = function
  | OpAdd -> ( + )
  | OpSub -> ( - )
  | OpMult -> ( * )
  | _ -> raise IllTyped

let prim2_bool : Exp.prim2 -> int -> int -> bool = function
  | OpGt -> ( > )
  | OpLt -> ( < )
  | OpEq -> ( = )
  | _ -> raise IllTyped

let rec eval_num (e : Exp.t) : Val.t = 
  match e with
  | NumLit n -> Num n
  | Prim1 (OpNeg, e) -> (
     let n = eval_num e in
     match n with
     | Num n -> Num (-n)
     | EffInv (name, args, ifer, body) ->
        EffInv(name, args, ifer, Prim1(OpNeg, body))
     | _ -> raise IllTyped
  )
  | Prim2 (e1, ((OpAdd | OpSub | OpMult) as op), e2) -> (
     let n1 = eval_num e1 in
     let n2 = eval_num e2 in
     let op_fun = prim2_num op in
     match (n1, n2) with
     | (EffInv(name, args, ifer, body),  _) ->
        EffInv(name, args, ifer, 
               Prim2(body, op, Val.to_expr n2))
     | (_,  EffInv(name, args, ifer, body)) ->
        EffInv(name, args, ifer, 
               Prim2(Val.to_expr n1, op, body))
     | (Num n1, Num n2) -> Num (op_fun n1 n2) 
     | _ -> raise IllTyped
  )
  | e -> (
     let v = eval e in
      match v with
      | Num _ -> v
      | EffInv _  -> v
      | _ -> raise IllTyped
  )
and eval_bool (e  : Exp.t) : Val.t =
  match e with 
  | BoolLit b -> Bool b
  | Prim1 (OpNot, e) -> (
     let b = eval_bool e in
     (*this probably needs to be refactored *)
     match b with
     | Bool b -> Bool (not b)
     | EffInv (name, args, ifer, body) ->
        EffInv(name, args, ifer, Prim1(OpNot, body))
     | _ -> raise IllTyped
  )
  | Prim2 (e1, ((OpGt | OpLt | OpEq) as op), e2)  -> (
     let n1 = eval_num e1 in
     let n2 = eval_num e2 in
     let op_fun = prim2_bool op in
     match (n1,n2) with
     | (EffInv(name, args, ifer, body),  _) ->
        EffInv(name, args, ifer,
               Prim2(body, op, Val.to_expr n2))
     | (_, EffInv(name, args, ifer, body)) ->
        EffInv(name, args, ifer,
               Prim2(Val.to_expr n1, op, body))
     | (Num n1, Num n2) -> Bool (op_fun n1 n2)
     | _ -> raise IllTyped
  )
  | e -> (
    let v = eval e in
      match v with
      | Bool _  -> v
      | EffInv _  -> v
      | _ -> raise IllTyped
  )
      
and eval (e : Exp.t) : Val.t =
  match e with
  | NumLit _ | Prim1 (OpNeg, _) | Prim2 (_, (OpAdd | OpSub |  OpMult), _) ->
     eval_num e
  | BoolLit _ | Prim1 (OpNot, _) | Prim2 (_, (OpLt | OpGt |  OpEq), _) ->
     eval_bool e
  | EffInv(name, args, ifer, body) -> EffInv(name, args, ifer, body)
  | Unit -> Triv
  | Var _->  raise IllTyped
  | Fun (var, body) -> Fun(var, body)
  | Handler elist -> Handler elist
  | Prim2 (e1, OpAp, e2) -> (
    match (eval e1) with
      | Fun (var, fun_body) -> (
        let e2' = Val.to_expr (eval e2) in
        match e2' with
        | EffInv (name, args, ifer, body) ->
           EffInv(name, args, ifer,
                  Prim2 (e1, OpAp, body))
        | _ -> eval (subst fun_body var e2')
      )
      | EffInv (name, args, ifer, body) -> 
           EffInv(name, args, ifer, Prim2 (body, OpAp, e2))
      | _ -> raise IllTyped
  )
  | If (e1, e2, e3) -> (
     let b = eval_bool e1 in
     match b with
     | Bool b ->  eval (if b then e2 else e3)
     | EffInv (name, args, ifer, body) ->
        EffInv(name, args, ifer,
               If(body, e2, e3))
     | _ -> raise IllTyped
  )
  | DoIn (ifer, bound, do_body) -> (
    let bound_v = Val.to_expr (eval bound) in
    match bound_v with
    | EffInv(name, args, ifer, body) ->
       EffInv(name, args, ifer,
              DoIn(ifer, body, do_body))
    | _ -> eval (subst do_body ifer bound_v)
  )
  | With (handler, h_body) -> (
    let handler_v = eval handler in
    match handler_v with
    | EffInv (name, args, ifer, body) ->
       EffInv(name, args, ifer,
              With(body, h_body))
    | Handler effects -> (
      let body_v = eval h_body in
      match body_v with
      | EffInv(name, args, ifer, cont) -> (
        match get_handler effects name with
        | Some effecth ->
           let (_, args, ifer, body) in
           eval (subst body ifer With(Val.to_expr handler_v, cont))
           
        | None -> 
           EffInv(name, args, ifer,
                  With (Val.to_expr handler_v, body))
      )
      (* no effects*) 
      | e -> body_v
    )
    | _ -> raise IllTyped
  )
