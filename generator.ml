open Batteries
open Parser

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
      Result.(return [Machine (Printf.sprintf "push %d" n)])
    | BinaryOp (i, op, left, right) ->
      Result.(let* lcom = aux left in
              let* rcom = aux right in
              let* com = match op with
                | Plus -> return [Machine "add rax, rdi"]
                | Minus -> return [Machine "sub rax, rdi"]
                | Mul -> return [Machine "imul rax, rdi"]
                | Div -> return [Machine "cqo";
                                 Machine "idiv rdi"]
                | Eq -> return [Machine "cmp rax, rdi";
                                Machine "sete al";
                                Machine "movzb rax, al"]
                | Neq -> return [Machine "cmp rax, rdi";
                                 Machine "setne al";
                                 Machine "movzb rax, al"]
                | Lt -> return [Machine "cmp rax, rdi";
                                Machine "setl al";
                                Machine "movzb rax, al"]
                | Le -> return [Machine "cmp rax, rdi";
                                Machine "setle al";
                                Machine "movzb rax, al"]
                | _ -> error @@ `GeneratorError (i, "invalid operator") in
              return @@ lcom @ rcom @ [Machine "pop rdi"; Machine "pop rax";] @ com @ [Machine "push rax"]) in
  Result.(let* commands = aux parsed in
          return @@ string_of_commands @@ [Assembler ".intel_syntax noprefix";
                                           Assembler ".global main"; Label "main"]
                                          @ commands
                                          @ [Machine "pop rax"; Machine "ret"])
