
module Ifer = struct
  type t = string
  let eq = (=)
end

module Exp = struct 
  type prim1 =
    | OpNot
    | OpNeg

  type prim2 =
    | OpAdd
    | OpSub
    | OpMult
    | OpEq
    | OpLt
    | OpGt
    | OpAp

  type effect =  (Ifer.t * Ifer.t list * Ifer.t * t)
  and t =
    | Unit 
    | BoolLit of bool
    | NumLit of int
    | Var of Ifer.t
    | Prim1 of prim1 * t
    | Prim2 of  t * prim2 * t
    | If of t * t * t
    | Fun of Ifer.t * t
    | DoIn of Ifer.t * t * t
    | With of t * t
    | EffInv of Ifer.t * t list * Ifer.t * t
    | Handler of effect list
end
                

