open Batteries
open Operator
module Result = Mresult

type pos = int

type t =
  | Reserved of pos * Operator.t
  | Num of pos * int
  | LParen of pos
  | RParen of pos

let atoi c = Char.code c - Char.code '0'

let to_string = function
  | Reserved (_, op) -> Printf.sprintf "Reserved(%s)" @@ Operator.to_string op
  | Num (_, n) -> Printf.sprintf "Num(%d)" n
  | LParen _ -> "LParen"
  | RParen _ -> "RParen"

let at = function
  | Reserved (p, _) -> p
  | Num (p, _) -> p
  | LParen p -> p
  | RParen p  -> p

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

let tokenize input =
  let rec aux = lazy
    State.(let+ i = get in
           if i = String.length input then
             return @@ Result.return []
           else
             let c = String.get input i in
             let+ () = next in
             match c with
             | '0' .. '9' ->
               let+ n = int input @@ atoi c in
               let+ ts = Lazy.force aux in
               return Result.(let* m = n in let* us = ts in return (m::us))
             | ' ' | '\t'  ->  Lazy.force aux
             | '+' | '-' | '*' | '/' | '=' | '!' | '<' | '>' ->
               let+ i = get in
               let c1 = String.get input i in
               begin match op_of_string @@ String.of_list [c; c1] with
                 | None ->
                   begin match op_of_char c with
                     | None ->
                       return @@ Result.error @@ `TokenizerError (i, "unexpected token")
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
               return  Result.(let* us = ts in
                               return @@ (LParen i::us))
             | ')' ->
               let+ ts = Lazy.force aux in
               return  Result.(let* us = ts in
                               return @@ (RParen i ::us))
             | _ -> return @@ Result.error @@ `TokenizerError (i, "unexpected token")) in
  State.evalState (Lazy.force aux) 0
