open Syntax
open Util

exception Runtime_error of string

type expval =
  IntV of int
| FunV of exp * (expval Environment.t)

let string_of_expval = function
  IntV n -> string_of_int n
| _ -> "<fun>"

let fun_v f env = FunV(f, env)

let apply_action f xs =
  let xs = List.map (fun x -> match x with
      IntV n -> n
    | _ -> raise (Runtime_error "wrong arguments")) xs in
  IntV (f xs)

let rec apply f xs = match f, xs with
| FunV (Zero, _), [] -> IntV 0
| FunV (Succ, _), [IntV n] -> IntV (n + 1)
| FunV (Proj(a, b), _), xs -> List.nth xs ((List.length xs) - a)
| FunV (Comp(g, fs), env), xs ->
    let xs = List.map (fun f -> apply (fun_v f env) xs) fs in
    apply (fun_v g env) xs
| FunV (PRec(h, g), env), (IntV 0)::xs -> apply (fun_v g env) xs
| FunV (PRec(h, g), env), (IntV n)::xs ->
    let y = IntV (n - 1) in
    let z = apply f (y::xs) in
    apply (fun_v h env) (z::y::xs)
| FunV (Var id, env), xs ->
    let f = (Environment.lookup id env) in
    apply f xs
| _, _ -> raise (Runtime_error "wrong application")

let rec eval env = function
  Int n -> IntV n
| Var id -> Environment.lookup id env
| App (f, xs) ->
    let f = eval env f
    and xs = List.map (eval env) xs in
    apply f (List.rev xs)
| LetExp (x, v, body) ->
    let v = eval env v in
    let env = Environment.extend x v env in
    eval env body
| Zero -> FunV (Zero, env)
| Succ -> FunV (Succ, env)
| Proj(x, y)  -> FunV(Proj(x, y), env)
| Comp(g, fs) -> FunV(Comp(g, fs), env)
| PRec(g, f)  -> FunV(PRec(g, f), env)
