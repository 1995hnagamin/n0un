open Syntax
open Util

exception Runtime_error of string

let rec apply_succ env = function
  Int n -> Int (n + 1)
| Var id ->
    let id = Environment.lookup id env in
    apply_succ env id
| _ -> raise (Runtime_error "Only integer succeeds")

let rec apply env f xs = 
  match f with
  Zero -> Int 0
| Succ -> apply_succ env (List.hd xs)
| Proj(n, _) -> List.nth xs (n - 1)
| Comp(g, fs) ->
    let xs = List.map (fun f -> apply env f xs) fs in
    apply env g xs
| PRec(g, f) -> apply_prec env g f (List.rev xs)
| Var id ->
    let f = Environment.lookup id env in
    apply env f xs
| _ -> raise (Runtime_error "Non-funtion object is applied")

and apply_prec env g f = function
  (Int 0)::xs -> apply env f (List.rev xs)
| (Int n)::xs ->
    let y = Int (n - 1) in
    let z = apply_prec env g f (y::xs) in
    apply env g (List.rev (z::y::xs))
| []  -> raise (Runtime_error "Arguments exhausted")
| _   -> raise (Runtime_error "Wrong arguments")

let rec eval env = function
  App(f, xs) -> apply env (eval env f) (List.map (eval env) xs)
| LetExp(x, v, body) ->
    let v = eval env v in
    let env = Environment.extend x v env in
    eval env body
| Var id -> eval env (Environment.lookup id env)
| x -> x
