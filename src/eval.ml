open Syntax
open Util

exception Runtime_error of string

let apply_succ = function
  Int n -> Int (n + 1)
| _ -> raise (Runtime_error "Only integer succeeds")

let rec apply f xs = 
  match f with
  Zero -> Int 0
| Succ -> apply_succ (List.hd xs)
| Proj(n, _) -> List.nth xs (n - 1)
| Comp(g, fs) ->
    let xs = List.map (fun f -> apply f xs) fs in
    apply g xs
| PRec(g, f) -> apply_prec g f (List.rev xs)
| _ -> raise (Runtime_error "Non-funtion object is applied")

and apply_prec g f = function
  (Int 0)::xs -> apply f (List.rev xs)
| (Int n)::xs ->
    let y = Int (n - 1) in
    let z = apply_prec g f (y::xs) in
    apply g (List.rev (z::y::xs))
| []  -> raise (Runtime_error "Arguments exhausted")
| _   -> raise (Runtime_error "Wrong arguments")

let rec eval env = function
  App(f, xs) -> apply (eval env f) (List.map (eval env) xs)
| LetExp(x, v, body) ->
    let v = eval env v in
    let env = Environment.extend x v env in
    eval env body
| x -> x
