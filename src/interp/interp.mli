fopen Sparse.Syntax;;
module Val : sig
  type t =
    | Num of int
    | Bool of bool
    | Fun of Ifer.t * Exp.t
    | Triv
  val to_expr : t -> Exp.t
end 

val eval : Exp.t -> Val.t
