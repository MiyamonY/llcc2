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

let (let>) = Result.Monad.bind
let tell = Result.ok % Writer.tell
let error = Result.error
let seq ls =
  List.fold_left (fun pred next ->
      let> p = pred in
      let> n = next in
      Result.Monad.return (p >>> n)) (tell []) ls

let machine_of_op op i =
  let open Operator in
  match op with
  | Plus -> tell [Machine "add rax, rdi"]
  | Minus -> tell [Machine "sub rax, rdi"]
  | Mul -> tell [Machine "imul rax, rdi"]
  | Div -> tell [Machine "cqo";
                 Machine "idiv rdi";]
  | Eq -> tell [Machine "cmp rax, rdi";
                Machine "sete al";
                Machine "movzb rax, al";]
  | Neq -> tell [Machine "cmp rax, rdi";
                 Machine "setne al";
                 Machine "movzb rax, al";]
  | Lt -> tell [Machine "cmp rax, rdi";
                Machine "setl al";
                Machine "movzb rax, al";]
  | Le -> tell [Machine "cmp rax, rdi";
                Machine "setle al";
                Machine "movzb rax, al";]
  | Assign -> tell [Machine "mov [rax], rdi";
                    Machine "push rdi"]
  | _ -> error @@ `GeneratorError (Some i, "invalid operator")

let generate_lval local = function
  | Variable(i, name) ->
    begin match Local.find local name with
      | None ->
        error @@ `GeneratorError (Some i, Printf.sprintf "variable %s not found" name)
      | Some n ->
        tell [Machine "mov rax, rbp";
              Machine (Printf.sprintf "sub rax, %d" @@ n);
              Machine "push rax"]
    end
  | _ as n ->
    error @@ `GeneratorError (Some (at n), Printf.sprintf "%s is not left value" @@ Parser.to_string n)

let init =
  tell [Assembler ".intel_syntax noprefix";
        Assembler ".global main";]

let prologue =
  tell [Machine "push rbp";
        Machine "mov rbp, rsp";]

let label name = tell [Label name]

let epiloge =
  tell [Machine "pop rax";
        Machine "mov rsp, rbp";
        Machine "pop rbp";
        Machine "ret"]

let move_arguments_to_stack local args =
  let regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";] in
  let maps = List.combine args @@ List.take (List.length args) regs in
  List.fold_left (fun pred (name, reg) ->
      match Local.find local name with
      | None ->
        error @@ `GeneratorError (None, Printf.sprintf "%s not found in local" @@ name)
      | Some n ->
        seq [pred;
             tell [Machine "mov rax, rbp";
                   Machine (Printf.sprintf "sub rax, %d" @@ n);
                   Machine "push rax";
                   Machine "pop rax";
                   Machine (Printf.sprintf "mov [rax], %s" reg)]])
    (tell []) maps

let assign_local_variable n =
  tell [Machine (Printf.sprintf "sub rsp, %d" n)]

let rec generate_node local = function
  | Number (_, n) -> tell [Machine (Printf.sprintf "push %d" n)]
  | BinaryOp (i, op, left, right) ->
    let left =
      (if op = Assign then generate_lval else generate_node) local left in
    seq [left;
         generate_node local right;
         tell [Machine "pop rdi"; Machine "pop rax";];
         machine_of_op op i;
         tell (if op = Assign then [] else [Machine "push rax"])]
  | FuncDecl (_, name, local, args, body) ->
    seq [label name;
         prologue;
         move_arguments_to_stack local args;
         assign_local_variable @@ Local.assign_size local;
         generate_nodes local body;
         epiloge]
  | Return(_, node) ->
    seq [generate_node local node;
         epiloge]
  | If(_, cond, then_, else_) ->
    let lelse = Label.create () in
    let lend = Label.create () in
    let else_ = match else_ with
      | None -> [label lend]
      | Some node ->
        [generate_node local node;
         label lend] in
    seq ([generate_node local cond;
          tell [Machine "pop rax";
                Machine "cmp rax, 0";
                Machine (Printf.sprintf "je %s" lelse)];
          generate_node local then_;
          tell [Machine (Printf.sprintf "jmp %s" lend)];
          label lelse;
         ] @ else_)
  | While(_, cond, body) ->
    let lbegin = Label.create ~label:"WhileBegin" () in
    let lend = Label.create ~label:"WhileEnd" () in
    seq [label lbegin;
         generate_node local cond;
         tell [Machine "pop rax";
               Machine "cmp rax, 0";
               Machine (Printf.sprintf "je %s" lend);];
         generate_node local body;
         tell [Machine (Printf.sprintf "jmp %s" lbegin)];
         label lend]
  | Variable(_, _) as n ->
    seq [generate_lval local n;
         tell [Machine "pop rax"; Machine "mov rax, [rax]";
               Machine "push rax"]]
  | For(_, init, cond, next, body) ->
    let lrep = Label.create ~label:"ForRep" () in
    let lend = Label.create ~label:"ForEnd" () in
    let init = match init with
      | None -> tell []
      | Some node -> generate_node local node in
    let cond = match cond with
      | None -> tell []
      | Some node -> generate_node local node in
    let next = match next with
      | None ->
        [tell [Machine (Printf.sprintf "jmp %s" lrep)]]
      | Some node ->
        [generate_node local node;
         tell [Machine (Printf.sprintf "jmp %s" lrep)]] in
    seq ([init;
          label lrep;
          generate_node local body;
          cond;
          tell [Machine "pop rax";
                Machine "cmp rax, 0";
                Machine (Printf.sprintf "je %s" lend);]]
         @ next
         @ [label lend])
  | Block(_, nodes) ->
    List.fold_left
      (fun pred node ->
         seq [pred;
              generate_node local node;
              tell [Machine "pop rax"];])
      (tell []) nodes
  | FuncCall(_, name, args)->
    let arguments args =
      let regs = ["rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";] in
      let maps = List.combine args @@ List.take (List.length args) regs in
      List.fold_left
        (fun pred (node, reg) ->
           seq [pred;
                generate_node local node;
                tell [Machine "pop rax"];
                tell [Machine (Printf.sprintf "mov %s, rax" reg)]])
        (tell []) @@ List.rev maps in
    seq [arguments args;
         tell [Machine (Printf.sprintf "call %s" name);
               Machine "push rax";]]

and generate_nodes local nodes =
  List.fold_left (fun pred node ->
      seq [pred;
           generate_node local node;])
    (tell []) nodes

let generate parsed =
  let> state =
    seq [init;
         generate_nodes (Local.create ()) parsed;] in
  Result.Monad.return @@ Writer.run state
  |> Result.map string_of_commands
