open Batteries

type token =
  | Reserved of char
  | Num of int

let atoi c = Char.code c - Char.code '0'

let rec parse_int n = function
  | [] -> (n, [])
  | c::rest ->
    match c with
    |  '0' .. '9' ->
      parse_int (n*10 + atoi c) rest
    | _ -> (n, c::rest)

exception Tokenize of string
let tokenize input =
  let rec aux = function
    | [] -> Ok []
    | c::rest ->
      match c with
      | '0' .. '9' ->
        let (n, r) = parse_int (atoi c) rest in
        Result.map (List.cons (Num n)) @@ aux r
      | ' ' | '\t' -> aux rest
      | '+' | '-' ->
        Result.map (List.cons (Reserved c)) @@ aux rest
      | _ as c -> Bad (Tokenize (Printf.sprintf "tokenize: invalid token %c" c ))
  in
  aux @@ String.to_list input

exception Generate of string

let expect_num tokens =
  match tokens with
  | [] -> Bad (Generate "generate: input exausted")
  | t::rest -> match t with
    | Num n -> Ok (n, rest)
    | _ -> Bad (Generate (Printf.sprintf "generate: unexpected token" ))

let (let*) m k =
  match m with
  | Ok x -> k x
  | Bad _  as e ->  e

let return x = Ok x

let generate tokens =
  let rec aux tokens =
    match tokens with
    | [] -> return []
    | Reserved o :: (Num n) :: rest ->
      let* ops  = aux rest in
      begin
        match o with
        | '+' -> return (Printf.sprintf "\tadd rax, %d" n :: ops)
        | '-' -> return (Printf.sprintf "\tsub rax, %d" n :: ops)
        | _ -> Bad (Generate "unexpected operator")
      end
    | _ -> Bad (Generate "token exhausted") in
  let* (n, rest) = expect_num tokens in
  let* parsed = aux rest in
  return @@ String.concat "\n" ([".intel_syntax noprefix";
                                 ".global main";
                                 "main:";
                                 Printf.sprintf "\tmov rax, %d" n]
                                @ parsed @ ["\tret"])

let () =
  (if Array.length Sys.argv != 2 then
     return "引数の個数が正しくありません"
   else
     let* tokens = tokenize Sys.argv.(1) in
     let* result = generate tokens in
     return result)
  |> Result.get
  |> Printf.printf "%s\n"
