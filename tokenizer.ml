open Batteries
open Operator
module Result = Mresult

type pos = int

type t =
  | Reserved of pos * Operator.t
  | Num of pos * int
  | LParen of pos
  | RParen of pos
  | Var of pos*string
  | Sep of pos
  | Return of pos

let atoi c = Char.code c - Char.code '0'

let to_string = function
  | Reserved (_, op) -> Printf.sprintf "Reserved(%s)" @@ Operator.to_string op
  | Num (_, n) -> Printf.sprintf "Num(%d)" n
  | LParen _ -> "LParen"
  | RParen _ -> "RParen"
  | Var (_, name) -> Printf.sprintf "Variable(%s)" name
  | Sep _ -> "Sep"
  | Return _ -> "Return"

let at = function
  | Reserved (p, _) -> p
  | Num (p, _) -> p
  | LParen p -> p
  | RParen p  -> p
  | Var (p, _)-> p
  | Sep p -> p
  | Return p -> p

let next =
  State.(let+ i = get in
         let+ () = put @@ i+1 in
         return ())

let rec int input n =
  State.(let+ i = get in
         if String.length input = i then
           return Result.(return @@ Num (i, n))
         else
           let c = String.get input i in
           match c with
           | '0' .. '9' ->
             let+ () = next in
             int input @@ 10 * n + atoi c
           | _ -> return Result.(return @@ Num (i, n)))

let rec string input chars =
  State.(let+ i = get in
         if String.length input = i then
           return Result.(return @@ String.of_list @@ List.rev chars)
         else
           let c = String.get input i in
           if Char.is_uppercase c || Char.is_lowercase c || Char.is_digit c then
             let+ () = next in
             string input @@ c::chars
           else
             return Result.(return @@ String.of_list @@ List.rev chars))

let tokenize input =
  let rec aux = lazy
    State.(let+ i = get in
           if i = String.length input then
             return @@ Result.return []
           else
             let c = String.get input i in
             let+ () = next in
             match c with
             | ' ' | '\t'  ->  Lazy.force aux
             | '+' | '-' | '*' | '/' | '=' | '!' | '<' | '>' ->
               let+ i = get in
               let c1 = String.get input i in
               begin match op_of_string @@ String.of_list [c; c1] with
                 | None ->
                   begin match op_of_char c with
                     | None ->
                       return @@ Result.error @@ `TokenizerError (Some i, "unexpected token")
                     | Some op ->
                       let+ ts = Lazy.force aux in
                       return Result.(let* us = ts in
                                      return @@ Reserved(i, op)::us)
                   end
                 | Some op ->
                   let+ () = next in
                   let+ ts = Lazy.force aux in
                   return Result.(let* us = ts in
                                  return @@ Reserved(i, op)::us)
               end
             | '(' ->
               let+ ts = Lazy.force aux in
               return Result.(let* us = ts in
                              return @@ (LParen i::us))
             | ')' ->
               let+ ts = Lazy.force aux in
               return Result.(let* us = ts in
                              return @@ (RParen i ::us))
             | ';' ->
               let+ ts = Lazy.force aux in
               return Result.(let* us = ts in
                              return @@ (Sep i :: us))
             | _ when Char.is_digit c ->
               let+ n = int input @@ atoi c in
               let+ ts = Lazy.force aux in
               return Result.(let* m = n in let* us = ts in return (m::us))
             | _ when Char.is_lowercase c || Char.is_uppercase c ->
               let+ str = string input [c] in
               let+ ts = Lazy.force aux in
               return Result.(
                   let* name = str in
                   let* us = ts in
                   match name with
                   | "return" -> return @@ Return i :: us
                   | _ -> return @@ (Var (i, name) :: us))
             | _ -> return @@ Result.error @@ `TokenizerError (Some i, "unexpected token"))
  in
  State.evalState (Lazy.force aux) 0
