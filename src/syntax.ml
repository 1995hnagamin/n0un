type id = string

type exp =
  Int of int
| App of exp * exp list
| Zero
| Succ
| Proj of (int * int)
| Comp of exp * (exp list)
| PRec of exp * exp

let rec string_of_exp = function
  Int n -> string_of_int n
| App (f, xs) ->
    let f = string_of_exp f
    and xs = List.map string_of_exp xs in
    f ^ "(" ^ String.concat "," xs ^ ")"
| Zero -> "zero"
| Succ -> "succ"
| Proj(x, y) -> "@" ^ string_of_int x ^ "/" ^ string_of_int y
| Comp(g, fs) ->
    let g = string_of_exp g
    and fs = List.map string_of_exp fs in
    g ^ "[" ^ String.concat "," fs ^ "]"
| PRec(g, f) -> string_of_exp g ^ "->" ^ string_of_exp f

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
