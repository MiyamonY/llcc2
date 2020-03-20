open Batteries
open Parser
open Writer.Infix

type command =
  | Assembler of string
  | Label of string
  | Machine of string

let to_string = function
  | Assembler com -> com
  | Label com -> Printf.sprintf "%s:" com
  | Machine com -> Printf.sprintf "\t%s" com

let string_of_commands = (String.concat "\n" % List.map to_string)

module Label = struct
  let num = ref 0

  let create ?(label="Label") ()=
    let label = Printf.sprintf "%s%d" label !num in
    num := !num+1;
    label
end

let machine_of_op op i =
  let open Operator in
  match op with
  | Plus -> Writer.tell [Machine "add rax, rdi"]
  | Minus -> Writer.tell [Machine "sub rax, rdi"]
  | Mul -> Writer.tell [Machine "imul rax, rdi"]
  | Div -> Writer.tell [Machine "cqo";
                        Machine "idiv rdi";]
  | Eq -> Writer.tell [Machine "cmp rax, rdi";
                       Machine "sete al";
                       Machine "movzb rax, al";]
  | Neq -> Writer.tell [Machine "cmp rax, rdi";
                        Machine "setne al";
                        Machine "movzb rax, al";]
  | Lt -> Writer.tell [Machine "cmp rax, rdi";
                       Machine "setl al";
                       Machine "movzb rax, al";]
  | Le -> Writer.tell [Machine "cmp rax, rdi";
                       Machine "setle al";
                       Machine "movzb rax, al";]
  | Assign -> Writer.tell [Machine "mov [rax], rdi";
                           Machine "push rdi"]
  | _ -> Writer.return @@ Result.error @@ `GeneratorError (Some i, "invalid operator")

let generate_lval = function
  | Variable(i, name) ->
    begin match Local.find name with
      | None ->
        Writer.return @@
        Result.error @@ `GeneratorError (Some i, Printf.sprintf "variable %s not found" name)
      | Some n ->
        Writer.tell [Machine "mov rax, rbp";
                     Machine (Printf.sprintf "sub rax, %d" @@ n);
                     Machine "push rax"]
    end
  | _ as n ->
    Writer.return @@
    Result.error @@ `GeneratorError (Some (at n), Printf.sprintf "%s is not left value" @@ Parser.to_string n)

let init =
  Writer.tell [Assembler ".intel_syntax noprefix";
               Assembler ".global main";]

let prolog =
  Writer.tell [Machine "push rbp";
               Machine "mov rbp, rsp";]

let epiloge =
  Writer.tell [Machine "pop rax";
               Machine "mov rsp, rbp";
               Machine "pop rbp";
               Machine "ret"]

let assign_local_variable n =
  Writer.tell [Machine (Printf.sprintf "sub rsp, %d" n)]

let rec generate_node = function
  | Number (_, n) -> Writer.tell [Machine (Printf.sprintf "push %d" n)]
  | BinaryOp (i, op, left, right) ->
    let@ result = (if op = Assign then generate_lval else generate_node) left in
    begin match result with
      | Error _ as err -> Writer.return err
      | Ok _ ->
        let@ result = generate_node right in
        begin match result with
          | Error _ as err -> Writer.return err
          | Ok _ ->
            Writer.tell [Machine "pop rdi"; Machine "pop rax";] >>>
            let@ result = machine_of_op op i in
            match result with
            | Error _ as err -> Writer.return err
            | Ok _ -> Writer.tell (if op = Assign then [] else [Machine "push rax"])
        end
    end
  | If(_, cond, then_, else_) ->
    let lelse = Label.create () in
    let lend = Label.create () in
    let@ _ = generate_node cond in
    Writer.tell [Machine "pop rax";
                 Machine "cmp rax, 0";
                 Machine (Printf.sprintf "je %s" lelse)] >>>
    let@ result = generate_node then_ in
    begin match result with
      | Error _ as err -> Writer.return err
      | Ok _ ->
        Writer.tell [Machine (Printf.sprintf "jmp %s" lend)] >>>
        Writer.tell [Label lelse] >>>
        match else_ with
        | None -> Writer.tell [Label lend]
        | Some node ->
          let@ result = generate_node node in
          match result with
          | Error _ as err -> Writer.return err
          | Ok _ -> Writer.tell [Label lend]
    end
  | Variable(_, _) as n ->
    let@ result = generate_lval n in
    begin match result with
      | Error _ as err -> Writer.return err
      | Ok _ ->
        Writer.tell [Machine "pop rax"; Machine "mov rax, [rax]"; Machine "push rax"]
    end
  | Return(_, node) ->
    begin let@ result = generate_node node in
      match result with
      | Error _ as err -> Writer.return err
      | Ok _ -> epiloge
    end
  | While(_, cond, body) ->
    let lbegin = Label.create ~label:"WhileBegin" () in
    let lend = Label.create ~label:"WhileEnd" () in
    Writer.tell [Label lbegin] >>>
    let@ result = generate_node cond in
    begin match result with
      | Error _ as err -> Writer.return err
      | Ok _ ->
        Writer.tell [Machine "pop rax";
                     Machine "cmp rax, 0";
                     Machine (Printf.sprintf "je %s" lend);] >>>
        let@ result = generate_node body in
        match result with
        | Error _ as err -> Writer.return err
        | Ok _ ->
          Writer.tell [Machine (Printf.sprintf "jmp %s" lbegin);
                       Label lend]
    end
  | For(_, init, cond, next, body) ->
    begin
      let lrep = Label.create ~label:"ForRep" () in
      let lend = Label.create ~label:"ForEnd" () in
      let@ result = match init with
        | None -> Writer.tell []
        | Some node -> generate_node node in
      match result with
      | Error _ as err -> Writer.return err
      | Ok _ ->
        Writer.tell [Label lrep] >>>
        let@ result = generate_node body in
        match result with
        | Error _ as err -> Writer.return err
        | Ok () ->
          let@ result = match cond with
            | None -> Writer.tell []
            | Some node -> generate_node node in
          match result with
          | Error _ as err -> Writer.return err
          | Ok _ ->
            Writer.tell [Machine "pop rax";
                         Machine "cmp rax, 0";
                         Machine (Printf.sprintf "je %s" lend);] >>>
            match next with
            | None -> Writer.tell
                        [Machine (Printf.sprintf "jmp %s" lrep);
                         Label lend]
            | Some node ->
              let@ result = generate_node node in
              match result with
              | Error _ as err -> Writer.return err
              | Ok _ ->
                Writer.tell [Machine (Printf.sprintf "jmp %s" lrep);
                             Label lend]
    end
  | Block(_, nodes) ->
    List.fold_left
      (fun pred node -> pred >>> generate_node node >>> Writer.tell [Machine "pop rax"])
      (Writer.return (Ok ())) nodes
  | FuncCall(_, name, args)->
    let arguments args =
      let regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";] in
      let maps = List.combine args @@ List.take (List.length args) regs in
      List.fold_left (fun pred (node, reg) -> pred >>>
                       let@ _ = generate_node node in
                       Writer.tell [Machine "pop rax"] >>>
                       Writer.tell [Machine (Printf.sprintf "mov %s, rax" reg)])
        (Writer.return (Ok ())) maps in
    arguments args >>>
    Writer.tell [Machine (Printf.sprintf "call %s" name);
                 Machine "push rax";]
  | FuncDecl (_, name, body) ->
    Writer.tell [Label name] >>>
    prolog >>>
    assign_local_variable @@ Local.assign_size () >>>
    let@ result = generate_nodes body in
    match result with
    | Error _ as err -> Writer.return err
    | Ok _ -> epiloge

and generate_nodes nodes =
  List.fold_left (fun pred node -> pred  >>> generate_node node) (Writer.return @@ (Ok ())) nodes

let generate parsed =
  let coms =
    init >>>
    let@ result = generate_nodes parsed in
    match result with
    | Error _ as err -> Writer.return err
    | Ok _ -> Writer.return (Ok ())
  in
  Writer.run coms
  |> Result.map string_of_commands
