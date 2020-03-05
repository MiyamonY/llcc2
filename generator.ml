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
  | Variable(i, name) ->
    begin match Local.find name with
      | None ->
        Result.(error @@ `GeneratorError (Some i, Printf.sprintf "variable %s not found" name))
      | Some n ->
        Result.(return [Machine "mov rax, rbp"; Machine (Printf.sprintf "sub rax, %d" @@ n);
                        Machine "push rax"])
    end
  | _ as n ->
    Result.(error @@ `GeneratorError (Some (at n), Printf.sprintf "%s is not left value" @@ print_node n))

let ret = [Machine "pop rax"; Machine "mov rsp, rbp"; Machine "pop rbp"; Machine "ret"]

let rec generate_node = function
  | Number (_, n) ->
    Result.(return [Machine (Printf.sprintf "push %d" n)])
  | BinaryOp (i, op, left, right) ->
    Result.(
      let* lcom =
        if op = Assign then generate_lval left
        else generate_node left in
      let* rcom = generate_node right in
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
        | _ -> error @@ `GeneratorError (Some i, "invalid operator") in
      return @@ lcom @ rcom @ [Machine "pop rdi"; Machine "pop rax";] @ com @
                if op = Assign then [] else [Machine "push rax"])
  | Variable(_, _) as n ->
    Result.(let* lcom = generate_lval n in
            return @@ lcom @ [Machine "pop rax"; Machine "mov rax, [rax]"; Machine "push rax"])
  | Return(_, node) ->
    Result.(let* com = generate_node node in
            return @@ com @ ret)

let rec generate_nodes = function
  | [] -> Result.(return [])
  | n::[] -> Result.(
      let* n = generate_node n in
      return n)
  | n::m::rest ->
    Result.(let* n = generate_node n in
            let* ns = generate_nodes (m::rest) in
            return @@ n @ ns)

let generate parsed =
  Result.(let* commands = generate_nodes parsed in
          [Assembler ".intel_syntax noprefix";
           Assembler ".global main"; Label "main";
           Machine "push rbp";
           Machine "mov rbp, rsp";
           Machine (Printf.sprintf "sub rsp, %d" @@ Local.assign_size ())]
          @ commands
          @ ret
          |> string_of_commands
          |> return)
