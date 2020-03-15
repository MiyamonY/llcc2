type ('a, 'e) t = ('a, 'e) BatPervasives.result = Ok of 'a | Error of 'e

val return : 'a -> ('a, 'b) t

val (let*) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

val error : 'b -> ('a, 'b) t

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) t -> 'c

val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t
