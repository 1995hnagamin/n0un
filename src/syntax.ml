type id = string

type exp =
  Int of int
| App of exp * exp list
| Zero
| Succ
| Proj of (int * int)
| Comp of exp * (exp list)
| PRec of exp * exp

type program = exp
;;

type ty =
  TyInt
| TyPFun of int (* primitive recursive function N^k -> N *)
| TyRFun of int (* recursive function N^k -> N *)

let string_of_ty = function
  TyInt -> "Int"
| TyPFun k -> "N^" ^ string_of_int k ^ " -> N, primitive recursive"
| TyRFun k -> "N^" ^ string_of_int k ^ " -> N, recursive"
