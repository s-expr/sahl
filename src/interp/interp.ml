exception NYI
exception IllTyped
open Sparse.Syntax;;

module Val = struct
  type t =
    | Num of int
    | Bool of bool
    | Fun of Ifer.t * Exp.t
    | Handler of Exp.effect list
    | Triv

  let to_expr value : Exp.t =
    match value with
    | Num i -> Exp.NumLit i
    | Bool b -> Exp.BoolLit b
    | Fun (var, body) -> Exp.Fun(var, body)
    | Handler elist -> Exp.Handler (elist)
    | Triv -> Exp.Unit
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

let rec eval (e : Exp.t) : Val.t =
  match e with
  | BoolLit b -> Bool b
  | NumLit i -> Num i
  | Unit -> Triv
  | Var ->  raise IllTyped
  | 
