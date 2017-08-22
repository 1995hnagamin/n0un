type t =
  Range of Somega.t * Somega.t
| Void

val range : Somega.t -> Somega.t -> t

val exact : int -> t

val at_least : int -> t

val inner : int -> t -> bool

val intersect : t -> t -> t

val to_string : t -> string
