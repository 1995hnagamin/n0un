type 'a t

exception Not_bound

val lookup : Syntax.id -> 'a t -> 'a

val update : Syntax.id -> 'a -> 'a t -> 'a t

val extend : Syntax.id -> 'a -> 'a t -> 'a t

val empty : 'a t
