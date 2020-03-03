type pos = int

type t =
  | Reserved of pos * Operator.t
  | Num of pos * int
  | LParen of pos
  | RParen of pos

val tokenize: string -> (t list, [> `TokenizerError of pos * string ]) BatResult.t

val at : t -> pos

val to_string : t -> string