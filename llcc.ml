open Batteries

let (let*) m k =
  match m with
  | Ok x -> k x
  | Bad _  as e ->  e

let return x = Ok x

let apply f g = function
  | Ok o -> f o
  | Bad b -> g b

type pos = int

type op =
  | Plus
  | Minus

let op_of_char = function
  | '+'-> Ok Plus
  | '-'-> Ok Minus
  | _ -> Bad "invalid token"

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
        let* op = op_of_char c in
        Result.map (List.cons (Reserved (i, op))) @@ aux (succ i) rest
      | _  -> Bad (error_message "tokenize" i "unexpected token")
  in
  aux 0 @@ String.to_list input

type node =
  | BinaryOp of pos * op * node * node
  | Number of pos * int

let primary =
  let open State in
  let+ tokens = get in
  match tokens with
  | [] -> return (Bad "token exhausted")
  | t :: rest->
    let+ () = put rest in
    match t with
    | Num (i, n) -> return (Ok (Number (i, n)))
    | _ -> return (Bad "unexpected token")

let mul = primary

let expr =
  let open State in
  let rec star left =
    let+ tokens = get in
    match tokens with
    | [] -> State.return left
    | t::rest ->
      let+ () = put rest in
      match t with
      | Reserved (i, op) ->
        let+ right = mul in
        let* lnode = left in
        let* rnode = right in
        State.return (BinaryOp (i, op, lnode, rnode))
      | _ -> State.return left in
  let+ r = mul in
  let* n = r in
  star n

let parse = expr

let rec generate  = function
  | Number (_, n) -> Ok ([Printf.sprintf "\tpush %d" n])
  | BinaryOp (_, op, left, right) ->
    let* lcom = generate left in
    let* rcom = generate right in
    match op with
    | Plus -> return @@ lcom  @ ["\tpop rdi"; "\tpop rax"; "\tadd rax, rdi"; "\tpush rax"] @ rcom
    | Minus -> return @@ lcom @ ["\tpop rdi"; "\tpop rax"; "\tsub rax, rdi"; "\tpush rax"] @ rcom

                                                                                             (* * let () =
                                                                                              * *   (if Array.length Sys.argv != 2 then
                                                                                              * *      Bad "引数の個数が正しくありません"
                                                                                              * *    else
                                                                                              * *      begin
                                                                                              * *        input := Sys.argv.(1);
                                                                                              * *        let* tokens = tokenize !input in
                                                                                              * *        let* (parsed, _) = parse tokens in
                                                                                              * *        let* commands = generate parsed in
                                                                                              * *        return @@ String.concat "\n" @@
                                                                                              * *        [".intel_syntax noprefix";
                                                                                              * *         ".global main"; "main:"] @ commands @ ["\tpop rax"; "\tret"]
                                                                                              * *      end)
                                                                                              * *   |> apply (fun s -> Printf.printf "%s\n" s; exit 0) (fun s -> Printf.eprintf "%s\n" s ; exit 1) *) *)
