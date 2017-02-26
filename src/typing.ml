open Syntax
open Util

exception Typing_error of string

let arity = function
  TyPFun n -> n
| TyRFun n -> n
| _ -> raise ( Typing_error "Not a function" )

let rec is_primitive env = function
  Zero -> true
| Succ -> true
| Proj(_,_) -> true
| Comp(g, fs) -> List.for_all (is_primitive env) (g::fs)
| PRec(g, f)  -> is_primitive env g && is_primitive env f
| Var id ->
    (match Environment.lookup id env with
      TyPFun _ -> true
    | TyRFun _ -> false
    | _ -> raise (Typing_error "Not a function"))
| _ -> raise ( Typing_error "Not a function" )

let arity_comp t_g t_fs =
  let k = arity t_g in
  let af = List.fold_left Arity.intersect (Arity.at_least 0) (List.map arity t_fs) in
  match af with
    Range.Void -> raise ( Typing_error "Arities of functions don't match" )
  | _ ->
    if not (Arity.is_applicable (List.length t_fs) k)
    then raise ( Typing_error "arity doesn't match" )
    else af

let ty_fun env f k = if is_primitive env f then TyPFun k else TyRFun k

let ar_prec x y = match(x, y) with
  (Range.Void, _) -> Range.Void
| (_, Range.Void) -> Range.Void
| (Range.Range(x, y), Range.Range(x', y')) ->
    let rec_range  = Range.range (Somega.max x (Somega.add 2 x')) (Somega.min y (Somega.add 2 y')) in
    let base_range = Range.range (Somega.max x' (Somega.sub 2 x)) (Somega.min y' (Somega.sub 2 y)) in
    match (rec_range, base_range) with
      (Range.Void, _) -> Range.Void
    | (_, Range.Void) -> Range.Void
    | (_, Range.Range(m, n)) ->
      Range.range (Somega.add 1 m) (Somega.add 1 n)

let rec eval_ty env = function
  Int _ -> TyInt
| Var x -> Environment.lookup x env
| App (f, xs) ->
    let k = arity (eval_ty env f) in
    (match () with
      _ when not (List.for_all (fun x -> eval_ty env x = TyInt) xs) ->
        raise (Typing_error "Non-integer object is applied")
    | _ when not (Arity.is_applicable (List.length xs) k) ->
        raise (Typing_error "Arity doesn't match")
    | _ -> TyInt)
| LetExp (x, v, body) ->
    let ty_v = eval_ty env v in
    let env' = Environment.extend x ty_v env in
    eval_ty env' body
| Zero -> typfun 0
| Succ -> typfun 1
| Proj(x, y) -> TyPFun y
| Comp(g, fs) ->
    let t_g  = eval_ty env g
    and t_fs = List.map (eval_ty env) fs in
    ty_fun  env (Comp(g, fs)) (arity_comp t_g t_fs)
| PRec(g, f) ->
    let y = arity (eval_ty env g)
    and x = arity (eval_ty env f) in
    let ar = ar_prec y x in
    match ar with
      Range.Void -> raise (Typing_error "Arity doesn't match")
    | _ -> ty_fun env (PRec (g, f)) ar
