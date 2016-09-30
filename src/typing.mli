open Syntax

exception Typing_error of string

val eval_ty : (ty Environment.t) -> exp -> ty
