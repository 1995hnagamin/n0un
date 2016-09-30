open Syntax
open Eval

type action =
  ActBind of (id * Eval.expval * ty)
| ActPrint of (Eval.expval * ty)

val exec :
  (action -> unit)
  -> (expval Environment.t)
  -> (ty Environment.t)
  -> program
  -> (expval Environment.t * ty Environment.t)
