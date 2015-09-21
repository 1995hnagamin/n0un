type 'a t = (Syntax.id * 'a) list
exception Not_bound

let empty = []

let rec lookup id = function
  [] -> raise Not_bound
| (x, v)::env' -> if (id = x) then v else lookup id env'
;;

let rec update id value = function
  [] -> [(id, value)]
| (x,v)::env' -> if (x = id) then (x, value)::env' else (x, v)::(update id value env')
;;

let extend id value env = (id,value)::env
