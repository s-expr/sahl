open Sparse.Syntax;;
module Val : sig
  type t =
    | Num of int
    | Bool of bool
    | Fun of Ifer.t * Exp.t
    | Triv
    | Handler of Exp.effectsig list
    | EffInv of Exp.effect
  val to_expr : t -> Exp.t
end 

val eval : Exp.t -> Val.t
