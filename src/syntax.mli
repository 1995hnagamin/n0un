module Arity : sig
  type t =
    Range of Somega.t * Somega.t
  | Null

  val exact : int -> t
  val at_least : int -> t
  val intersect : t -> t -> t
  val is_applicable : int -> t -> bool
end

type id = string

type ty =
  TyInt
| TyPFun of int (* primitive recursive function N^k -> N *)
| TyRFun of int (* recursive function N^k -> N *)

type exp =
  Int of int
| Var of id
| App of exp * exp list
| LetExp of id * exp * exp
| Zero
| Succ
| Proj of (int * int)
| Comp of exp * (exp list)
| PRec of exp * exp

type stmt =
  PrintStmt of exp
| LetDecl of id * exp

type program = stmt list

val string_of_ty : ty -> string

val string_of_exp : exp -> string
