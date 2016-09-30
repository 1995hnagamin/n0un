type t =
  Num of int
| Infty

val ge : t -> t -> bool
val le : t -> t -> bool
val less : t -> t -> bool
val greater : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val add : int -> t -> t
val sub : int -> t -> t
val to_string : t -> string
