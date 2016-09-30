module Arity = struct
  (* [a, b) *)
  type t =
    Range of Somega.t * Somega.t
  | Null

  let exact n =
    Range(Somega.Num n, Somega.Num (n + 1))

  let at_least n =
    Range(Somega.Num n, Somega.Infty)

  let is_applicable n = function
    Range (a, b) ->
      let n' = Somega.Num n in
      (Somega.le a n') && (Somega.less n' b)
  | Null -> false

  let intersect x y = match (x, y) with
    (Range(x, y), Range(x', y')) ->
      let (m, n) = (max x x', min y y') in
      if Somega.less m n then Range(m, n) else Null
  | _ -> Null

  let to_string = function
    Range (x, y) ->
      "[" ^ (Somega.to_string x) ^ "," ^ (Somega.to_string y) ^ ")"
  | Null -> "()"
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
