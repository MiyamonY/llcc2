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

let print_op = function
  | Plus -> "+"
  | Minus -> "-"

let op_of_char = function
  | '+'-> Ok Plus
  | '-'-> Ok Minus
  | _ -> Error "invalid token"

type token =
  | Reserved of pos * op
  | Num of pos * int

let input = ref ""

let atoi c = Char.code c - Char.code '0'

let rec parse_int i n = function
  | [] -> (i, n, [])
  | c::rest ->
    match c with
    |  '0' .. '9' ->
      parse_int (succ i) (n*10 + atoi c) rest
    | _ -> (i, n, c::rest)

let error_message title n msg =
  let sep = String.repeat " " n in
  Printf.sprintf "%s:\n%s\n%s^ %s" title !input sep msg

let tokenize input =
  let rec aux i = function
    | [] -> Ok []
    | c::rest ->
      match c with
      | '0' .. '9' ->
        let (i, n, r) = parse_int (succ i) (atoi c) rest in
        Result.map (List.cons (Num (i, n))) @@ aux i r
      | ' ' | '\t' -> aux (succ i) rest
      | '+' | '-' ->
        Result.(let* op = op_of_char c in
                map (List.cons (Reserved (i, op))) @@ aux (succ i) rest)
      | _  -> Error (error_message "tokenize" i "unexpected token")
  in
  aux 0 @@ String.to_list input

type node =
  | Number of pos * int
  | BinaryOp of pos * op * node * node

let rec print_node = function
  | Number(_, n)  -> Printf.sprintf "Number(%d)" n
  | BinaryOp(_, op, l, r) ->
    let sl = print_node l in
    let sr = print_node r in
    Printf.sprintf "BinaryOp(%s,%s,%s)\n" (print_op op) sl sr

let primary =
  State.(let+ tokens = get in
         match tokens with
         | [] -> return (Error "token exhausted")
         | t :: rest->
           let+ () = put rest in
           match t with
           | Num (i, n) -> return (Ok (Number (i, n)))
           | _ -> return (Error "unexpected token"))

let mul = primary

(* expr  = mul ("+" mul | "-" mul)* *)
let expr =
  let rec star left =
    State.(let+ tokens = get in
           match tokens with
           | [] -> return left
           | t::rest ->
             let+ () = put rest in
             match t with
             | Reserved (i, op) ->
               let+ right = mul in
               let n = Result.(let* lnode = left in
                               let* rnode = right in
                               return @@ BinaryOp (i, op, lnode, rnode)) in
               star n
             | _ -> return left) in
  State.(let+ left = mul in
         star left)

(* program = expr *)
let program = expr

let parse = program

let rec generate  = function
  | Number (_, n) -> Ok ([Printf.sprintf "\tpush %d" n])
  | BinaryOp (_, op, left, right) ->
    Result.(let* lcom = generate left in
            let* rcom = generate right in
            match op with
            | Plus -> return @@ lcom  @ rcom @  ["\tpop rdi"; "\tpop rax"; "\tadd rax, rdi"; "\tpush rax"]
            | Minus -> return @@ lcom @ rcom @ ["\tpop rdi"; "\tpop rax"; "\tsub rax, rdi"; "\tpush rax"])

let () =
  (if Array.length Sys.argv != 2 then
     Error "引数の個数が正しくありません"
   else
     begin
       input := Sys.argv.(1);
       Result.(let* tokens = tokenize !input in
               let (parsed, _) = State.runState parse tokens in
               let* p = parsed in
               let* commands = generate p in
               return @@ String.concat "\n" @@
               [".intel_syntax noprefix";
                ".global main"; "main:"] @ commands @ ["\tpop rax"; "\tret"])
     end)
  |> Result.fold ~ok:(fun s -> Printf.printf "%s\n" s;0) ~error:(fun s -> Printf.eprintf "%s\n" s ;1)
  |> exit
