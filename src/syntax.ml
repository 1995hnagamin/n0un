type id = string

type exp =
  Int of int
| App of exp * exp list
| Zero
| Succ
| Proj of (int * int)
| Comp of exp * (exp list)
| PRec of exp * exp

type program = exp
;;
