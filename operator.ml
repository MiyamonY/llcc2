type t =
  | Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge

let to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let op_of_string = function
  | "+"-> Some Plus
  | "-" -> Some Minus
  | "*" -> Some Mul
  | "/" -> Some Div
  | "<" -> Some Lt
  | "<=" -> Some Le
  | "==" -> Some Eq
  | "!=" -> Some Neq
  | ">" -> Some Gt
  | ">=" -> Some Ge
  | _ -> None

let op_of_char = function
  | '+'-> Some Plus
  | '-'-> Some Minus
  | '*' -> Some Mul
  | '/' -> Some Div
  | '<' -> Some Lt
  | '>' -> Some Gt
  | _ -> None
