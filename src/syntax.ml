module Arity = struct
  (* [a, b) *)
  type t = (Somega.t * Somega.t)

  let exact n =
    (Somega.Num n, Somega.Num (n + 1))

  let at_least n =
    (Somega.Num n, Somega.Infty)

  let is_applicable n (a, b) =
    let n' = Somega.Num n in
    (Somega.le a n') && (Somega.less n' b)

  let intersect (x, y) (x', y') =
    (max x x', min y y')
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
| Proj(x, y) -> "@" ^ string_of_int x ^ "/" ^ string_of_int y
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
| TyPFun k -> "N^" ^ string_of_int k ^ " -> N, primitive recursive"
| TyRFun k -> "N^" ^ string_of_int k ^ " -> N, recursive"
