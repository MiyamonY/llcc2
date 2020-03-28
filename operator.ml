open Batteries

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
  | Ref
  | Deref

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
  | Ref -> "&"
  | Deref -> "*"

let of_string c =
  match String.of_char c with
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
