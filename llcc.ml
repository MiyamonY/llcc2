open Batteries

module Result = struct
  include Result

  let (let*) m k =
    match m with
    | Ok x -> k x
    | Error _  as e ->  e

  let return x = Ok x
end

type pos = int

type op =
  | Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Neq

let print_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Neq -> "!="

let op_of_char = function
  | '+'-> Some Plus
  | '-'-> Some Minus
  | '*' -> Some Mul
  | '/' -> Some Div
  | _ -> None

type token =
  | Reserved of pos * op
  | Num of pos * int
  | LParen of pos
  | RParen of pos

let string_of_token token =
  match token with
  | Reserved (_, op) -> Printf.sprintf "Reserved(%s)" @@ print_op op
  | Num (_, n) -> Printf.sprintf "Num(%d)" n
  | LParen _ -> "LParen"
  | RParen _ -> "RParen"

let token_pos = function
  | Reserved (p, _) -> p
  | Num (p, _) -> p
  | LParen p -> p
  | RParen p  -> p

let input = ref ""

let atoi c = Char.code c - Char.code '0'

let error_message  = function
  | `ArgumentError msg ->
    Printf.sprintf "Argument error: %s" msg
  | `TokenizerError (i, msg) ->
    let sep = String.repeat " " i in
    Printf.sprintf "Tokenizer error:\n%s\n%s^ %s" !input sep msg
  | `ParserError (i, msg) ->
    let sep = String.repeat " " i in
    Printf.sprintf "Parser error:\n%s\n%s^ %s" !input sep msg

let rec int n =
  State.(let+ i = get in
         if String.length !input = i then
           return Result.(return @@ Num (i, n))
         else
           let c = String.get !input i in
           match c with
           | '0' .. '9' ->
             let+ () = put @@ i+1 in
             int @@ 10 * n + atoi c
           | _ -> return Result.(return @@ Num (i, n)))

let tokenize input =
  let rec aux = lazy
    State.(let+ i = get in
           if i = String.length input then
             return @@ Result.return []
           else
             let c = String.get input i in
             let+ () = put (i + 1) in
             match c with
             | '0' .. '9' ->
               let+ n = int @@ atoi c in
               let+ ts = Lazy.force aux in
               return Result.(let* m = n in let* us = ts in return (m::us))
             | ' ' | '\t'  ->  Lazy.force aux
             | '+' | '-' | '*' | '/' | '=' | '!' ->
               begin match op_of_char c with
                 | None ->
                   let+ i = get in
                   let c1 = String.get input i in
                   begin match c, c1 with
                     | '=', '=' ->
                       let+ () = put (i+1) in
                       let+ ts = Lazy.force aux in
                       return Result.(let* us = ts in
                                      return @@ Reserved(i, Eq)::us)
                     | '!',  '=' ->
                       let+ () = put (i+1) in
                       let+ ts = Lazy.force aux in
                       return Result.(let* us = ts in
                                      return @@ Reserved(i, Neq):: us)
                     | _, _ ->
                       return @@ Result.error @@ `TokenizerError (i, "unexpected token")
                   end
                 | Some op ->
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

type node =
  | Number of pos * int
  | BinaryOp of pos * op * node * node

let rec print_node = function
  | Number(_, n)  -> Printf.sprintf "Number(%d)" n
  | BinaryOp(_, op, l, r) ->
    let sl = print_node l in
    let sr = print_node r in
    Printf.sprintf "BinaryOp(%s,%s,%s)\n" (print_op op) sl sr

let peek =
  State.(let+ tokens = get in
         match tokens with
         | [] -> return None
         | t:: _ -> return @@ Some t)

let next =
  State.(let+ tokens = get in
         match tokens with
         | [] -> return @@ Result.error @@ `ParserError (String.length !input, "token exhausted")
         | _::rest ->
           let+ () = put rest in return Result.(return ()))

(* primary = num | "(" expr ")" *)
let rec primary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ Result.error @@ `ParserError (String.length !input, "token exhausted")
         | Some token ->
           match token with
           | Num (i, n) ->
             let+ _ = next in
             return Result.(return @@ Number (i, n))
           | LParen _ ->
             let+ _ = next in
             let+ e = Lazy.force expr in
             let+ token = peek in
             begin
               match token with
               | None -> return @@ Result.error @@ `ParserError (String.length !input, "token exhausted")
               | Some t ->
                 match t with
                 | RParen _ -> let+ _ = next in return e
                 | _   -> return @@ Result.error @@
                   `ParserError (token_pos t, Printf.sprintf "unexpected token: %s" @@ string_of_token t)
             end
           | _ -> return @@ Result.error @@
             `ParserError (token_pos token, Printf.sprintf "unexpected token: %s" @@ string_of_token token))

(* unary = ("+" | "-")? primary *)
and unary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ Result.error @@ `ParserError (String.length !input, "token exhausted")
         | Some token ->
           match token with
           | Reserved (i, op) ->
             begin match op with
               | Plus ->
                 let+ _ = next in
                 Lazy.force primary
               | Minus ->
                 let+ _ = next in
                 let+ node = Lazy.force primary in
                 return Result.(
                     let* n = node in
                     return @@ BinaryOp(i, Minus, Number (i, 0), n))
               | _ -> Lazy.force primary
             end
           | _ -> Lazy.force primary)

(* mul = unary ("*" unary | "/" unary)* *)
and mul =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Mul | Div ->
                   let+ _ = next in
                   let+ right = Lazy.force unary in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return @@ BinaryOp (i, op, lnode, rnode)) in
                   star n
                 | _ -> return left
               end
             | _ -> return left) in
  lazy State.(let+ left =  Lazy.force unary in
              star left)

(* add  = mul ("+" mul | "-" mul)* *)
and add =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Plus | Minus ->
                   let+ _ = next in
                   let+ right = Lazy.force mul in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return @@ BinaryOp (i, op, lnode, rnode)) in
                   star n
                 | _ -> return left
               end
             | _ -> return left) in
  lazy State.(let+ left = Lazy.force mul in
              star left)

(* equality = add ("==" add | "!=" add) * *)
and equality =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Eq | Neq ->
                   let+ _ = next in
                   let+ right = Lazy.force add in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return @@ BinaryOp (i, op, lnode, rnode)) in
                   star n
                 | _ ->
                   return left
               end
             | _ -> return left) in
  lazy State.(let+ left = Lazy.force add in
              star left)

(*  expr = add *)
and expr = lazy
  (Lazy.force equality)

(* program = expr *)
and program = lazy (Lazy.force expr)

let parse = Lazy.force program

type command =
  | Assembler of string
  | Label of string
  | Machine of string

let string_of_command = function
  | Assembler com -> com
  | Label com -> Printf.sprintf "%s:" com
  | Machine com -> Printf.sprintf "\t%s" com

let string_of_commands = (String.concat "\n" % List.map string_of_command)

let generate parsed =
  let rec aux = function
    | Number (_, n) ->
      [Machine (Printf.sprintf "push %d" n)]
    | BinaryOp (_, op, left, right) ->
      let lcom = aux left in
      let rcom = aux right in
      let op = match op with
        | Plus -> [Machine "add rax, rdi"]
        | Minus -> [Machine "sub rax, rdi"]
        | Mul -> [Machine "imul rax, rdi"]
        | Div -> [Machine "cqo";
                  Machine "idiv rdi"]
        | Eq -> [Machine "cmp rax, rdi";
                 Machine "sete al";
                 Machine "movzb rax, al"]
        | Neq -> [Machine "cmp rax, rdi";
                  Machine "setne al";
                  Machine "movzb rax, al"] in
      lcom @ rcom @ [Machine "pop rdi"; Machine "pop rax";] @ op @ [Machine "push rax"] in
  string_of_commands @@ [Assembler ".intel_syntax noprefix";
                         Assembler ".global main"; Label "main"]
                        @ aux parsed
                        @ [Machine "pop rax"; Machine "ret"]

let () =
  (if Array.length Sys.argv != 2 then
     Result.error (`ArgumentError "The number of arugument is 1")
   else
     begin
       input := Sys.argv.(1);
       Result.(let* tokens = tokenize !input in
               let (parsed, _) = State.runState parse tokens in
               let* p = parsed in
               return @@ generate p)
     end)
  |> Result.fold ~ok:(fun s -> Printf.printf "%s\n" s;0)
    ~error:(fun s -> Printf.eprintf "%s\n" @@ error_message s ;1)
  |> exit
