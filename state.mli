type ('a, 'b) t

val return : 'a -> ('a, 'b) t

val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

val (let+) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

val execState : ('a, 'b) t -> 'b -> 'b

val evalState : ('a, 'b) t -> 'b -> 'a

val runState : ('a, 'b) t -> 'b -> ('a * 'b)

val get : ('a, 'a) t

val put : 'a -> (unit, 'a) t
