open Syntax

val is_same_as : ('a -> 'b) -> 'a list -> bool

val is_different : ('a -> 'b) -> 'a list -> bool

val (<<) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val comp : exp -> exp -> exp

val app : exp -> int -> exp

val apps : exp -> int list -> exp

val proj : int -> int -> exp
