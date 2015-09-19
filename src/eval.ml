open Syntax

let rec eval f = match f with
  Zero -> "Zero"
| Succ -> "Succ"
| Proj (x, y) -> "(Proj " ^ string_of_int x ^ " " ^ string_of_int y ^ ")"
| Comp (g, fs) -> "(Comp " ^ eval g ^ " " ^ (String.concat " " (List.map eval fs)) ^ ")"
| PRec (g, f) -> "(PRec " ^ eval g ^ " " ^ eval f ^ ")"
| Int i -> string_of_int i
| App (f, xs) -> "(App " ^ eval f ^ " " ^ String.concat " " (List.map eval xs) ^ ")" 
