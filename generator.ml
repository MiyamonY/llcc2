open Batteries
open Parser

type command =
  | Assembler of string
  | Label of string
  | Machine of string

let to_string = function
  | Assembler com -> com
  | Label com -> Printf.sprintf "%s:" com
  | Machine com -> Printf.sprintf "\t%s" com

let string_of_commands = (String.concat "\n" % List.map to_string)

let offset c =
  8*(Char.code c - Char.code 'a')

let generate_lval = function
  | Variable(_, c) ->
    Result.(return [Machine "mov rax, rbp"; Machine (Printf.sprintf "sub rax, %d" @@ offset c); Machine "push rax"])
  | _ as n ->
    Result.(error @@ `GeneratorError (at n, Printf.sprintf "%s is not left value" @@ print_node n))

let generate parsed =
  let rec aux = function
    | Number (_, n) ->
      Result.(return [Machine (Printf.sprintf "push %d" n)])
    | BinaryOp (i, op, left, right) ->
      Result.(
        let* lcom =
          if op = Assign then generate_lval left
          else aux left in
        let* rcom = aux right in
        let* com = match op with
          | Plus -> return [Machine "add rax, rdi"]
          | Minus -> return [Machine "sub rax, rdi"]
          | Mul -> return [Machine "imul rax, rdi"]
          | Div -> return [Machine "cqo";
                           Machine "idiv rdi";]
          | Eq -> return [Machine "cmp rax, rdi";
                          Machine "sete al";
                          Machine "movzb rax, al";]
          | Neq -> return [Machine "cmp rax, rdi";
                           Machine "setne al";
                           Machine "movzb rax, al";]
          | Lt -> return [Machine "cmp rax, rdi";
                          Machine "setl al";
                          Machine "movzb rax, al";]
          | Le -> return [Machine "cmp rax, rdi";
                          Machine "setle al";
                          Machine "movzb rax, al";]
          | Assign -> return [Machine "mov [rax], rdi"; Machine "push rdi"]
          | _ -> error @@ `GeneratorError (i, "invalid operator") in
        return @@ lcom @ rcom @ [Machine "pop rdi"; Machine "pop rax";] @ com @
                  if op = Assign then [] else [Machine "push rax"])
    | Variable(_, _) as n ->
      Result.(let* lcom = generate_lval n in
              return @@ lcom @ [Machine "pop rax"; Machine "mov rax, [rax]"; Machine "push rax"])
  in
  Result.(let* commands = aux parsed in
          [Assembler ".intel_syntax noprefix";
           Assembler ".global main"; Label "main";
           Machine "push rbp";
           Machine "mov rbp, rsp";
           Machine "sub rsp, 208"]
          @ commands
          @ [Machine "pop rax"; Machine "mov rsp, rbp"; Machine "pop rbp"; Machine "ret"]
          |> string_of_commands
          |> return)
