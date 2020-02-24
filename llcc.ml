open Batteries

type pos = int

type token =
  | Reserved of pos * char
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
        Result.map (List.cons (Reserved (i, c))) @@ aux (succ i) rest
      | _  -> Bad (error_message "tokenize" i "unexpected token")
  in
  aux 0 @@ String.to_list input

let expect_num tokens =
  match tokens with
  | [] -> Bad "generate: input exausted"
  | t::rest -> match t with
    | Num (_, n) -> Ok (n, rest)
    | Reserved (i, _) -> Bad (error_message "generate" i "unexpected token")

let (let*) m k =
  match m with
  | Ok x -> k x
  | Bad _  as e ->  e

let return x = Ok x

let apply f g = function
  | Ok o -> f o
  | Bad b -> g b

let generate tokens =
  let rec aux tokens =
    match tokens with
    | [] -> return []
    | Reserved (i, o) :: Num (_, n) :: rest ->
      let* ops  = aux rest in
      begin
        match o with
        | '+' -> return (Printf.sprintf "\tadd rax, %d" n :: ops)
        | '-' -> return (Printf.sprintf "\tsub rax, %d" n :: ops)
        | _ -> Bad (error_message "generate" i "unexpected token")
      end
    | _ -> Bad (error_message "generate" (String.length !input) "token exhausted") in
  let* (n, rest) = expect_num tokens in
  let* parsed = aux rest in
  return @@ [".intel_syntax noprefix";
             ".global main";
             "main:";
             Printf.sprintf "\tmov rax, %d" n]
            @ parsed @ ["\tret"]

let () =
  (if Array.length Sys.argv != 2 then
     Bad "引数の個数が正しくありません"
   else
     begin
       input := Sys.argv.(1);
       let* tokens = tokenize !input in
       let* commands = generate tokens in
       return @@ String.concat "\n" commands
     end)
  |> apply (fun s -> Printf.printf "%s\n" s; exit 0) (fun s -> Printf.eprintf "%s\n" s ; exit 1)
