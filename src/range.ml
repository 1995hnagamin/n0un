type t =
  Range of Somega.t * Somega.t
| Void

let range x y =
  if Somega.less x y
  then Range(x, y)
  else Void

let exact n =
  Range(Somega.Num n, Somega.Num(n + 1))

let at_least n =
  Range(Somega.Num n, Somega.Infty)

let inner i n =
  let i = Somega.Num i in
  match n with
    Range(x, y) -> (Somega.le x i) && (Somega.less i y)
  | Void -> false

let intersect a b = match (a, b) with
  (Void, _) -> Void
| (_, Void) -> Void
| (Range(x, y), Range(x', y')) ->
    let m = Somega.max x x' in
    let n = Somega.min y y' in
    if (Somega.less m n)
    then Range(m, n)
    else Void

let to_string = function
  Range(x, y) ->
    "[" ^ (Somega.to_string x) ^ ", " ^ (Somega.to_string y) ^ ")"
| Void -> "()"
