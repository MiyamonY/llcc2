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
  | Assign

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
  | Assign -> "="

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
  | '=' -> Some Assign
  | _ -> None
