open Syntax

exception Runtime_error of string

type expval =
  IntV of int
| FunV of exp * (expval Environment.t)

val string_of_expval : expval -> string

val eval : (expval Environment.t) -> exp -> expval
