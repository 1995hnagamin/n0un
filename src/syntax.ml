module Arity = struct
  type t = Range.t

  let is_applicable n arity =
    Range.inner n arity

  let exact = Range.exact
  let at_least = Range.at_least
  let intersect = Range.intersect
  let to_string = Range.to_string
end

type id = string

type ty =
  TyInt
| TyPFun of Arity.t (* primitive recursive function N^k -> N *)
| TyRFun of Arity.t (* recursive function N^k -> N *)

type exp =
  Int of int
| Var of id
| App of exp * exp list
| LetExp of id * exp * exp
| Zero
| Succ
| Proj of (int * Arity.t)
| Comp of exp * (exp list)
| PRec of exp * exp

let rec string_of_exp = function
  Int n -> string_of_int n
| Var x -> x
| App (f, xs) ->
    let f = string_of_exp f
    and xs = List.map string_of_exp xs in
    f ^ "(" ^ String.concat "," xs ^ ")"
| LetExp (x, v, body) ->
    let v = string_of_exp v
    and body = string_of_exp body in
    "let " ^ x ^ "=" ^ v ^ " in " ^ body
| Zero -> "zero"
| Succ -> "succ"
| Proj(x, y) -> "@" ^ string_of_int x ^ "/" ^ Arity.to_string y
| Comp(g, fs) ->
    let g = string_of_exp g
    and fs = List.map string_of_exp fs in
    g ^ "[" ^ String.concat "," fs ^ "]"
| PRec(g, f) -> string_of_exp g ^ "->" ^ string_of_exp f

type stmt =
  PrintStmt of exp
| LetDecl of id * exp

type program = stmt list

let string_of_ty = function
  TyInt -> "Int"
| TyPFun k -> "N^" ^ Arity.to_string k ^ " -> N, primitive recursive"
| TyRFun k -> "N^" ^ Arity.to_string k ^ " -> N, recursive"
