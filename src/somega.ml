type t =
  Num of int
| Infty

let ge x y = match (x, y) with
  (Num x, Num y) -> x >= y
| (Num _, Infty) -> false
| (Infty, _) -> true

let le x y =
  ge y x

let less x y =
  not (ge x y)

let greater x y =
  not (le x y)

let add i = function
  Num n -> Num (n + i)
| Infty -> Infty

let sub i = function
  Num n -> Num (min 0 (n - i))
| Infty -> Infty

let min x y =
  if le x y then x else y

let max x y =
  if ge x y then x else y

let to_string = function
  Num n -> string_of_int n
| Infty -> "oo"
