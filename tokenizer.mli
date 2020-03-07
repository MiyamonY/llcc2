type pos = int

type t =
  | Reserved of pos * Operator.t
  | Num of pos * int
  | LParen of pos
  | RParen of pos
  | Var of pos * string
  | Sep of pos
  | Return of pos
  | If of pos
  | Else of pos
  | While of pos

val tokenize: string -> (t list, [> `TokenizerError of pos option * string ]) BatResult.t

val at : t -> pos

val to_string : t -> string
