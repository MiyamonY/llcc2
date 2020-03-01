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
  | LParen of pos
  | RParen of pos

let input = ref ""

let atoi c = Char.code c - Char.code '0'

let error_message title n msg =
  let sep = String.repeat " " n in
  Printf.sprintf "%s:\n%s\n%s^ %s" title !input sep msg

let rec int n =
  State.(let+ i = get in
         if String.length !input = i then
           return @@ Ok (Num (i, n))
         else
           let c = String.get !input i in
           match c with
           | '0' .. '9' ->
             let+ () = put @@ i+1 in
             int @@ 10 * n + atoi c
           | _ -> return @@ Ok (Num (i, n)))

let tokenize input =
  let rec aux = lazy
    State.(let+ i = get in
           if i = String.length input then
             return @@ Result.return []
           else
             let c = String.get input i in
             let+ () = put @@ i + 1 in
             match c with
             | '0' .. '9' ->
               let+ n = int @@ atoi c in
               let+ ts = Lazy.force aux in
               return Result.(let* m = n in let* us = ts in return (m::us))
             | ' ' | '\t'  ->  Lazy.force aux
             | '+' | '-' ->
               let+ ts = Lazy.force aux in
               let tokens = Result.(let* op = op_of_char c in
                                    let* us = ts in
                                    return @@ (Reserved(i, op))::us) in
               return tokens
             | '(' ->
               let+ ts = Lazy.force aux in
               let tokens = Result.(let* us = ts in
                                    return @@ (LParen i ::us)) in
               return tokens
             | ')' ->
               let+ ts = Lazy.force aux in
               let tokens = Result.(let* us = ts in
                                    return @@ (RParen i ::us)) in
               return tokens
             | _ -> return @@ Result.Error (error_message "tokenize" i "unexpected token")
          ) in
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
         | [] -> return @@ Error "token exhausted"
         | _::rest ->
           let+ () = put rest in return @@ Ok ())

(* primary = num | "(" expr ")" *)
let rec primary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ Error "token exhausted"
         | Some token ->
           match token with
           | Num (i, n) ->
             let+ _ = next in return @@ Ok (Number (i, n))
           | LParen _ ->
             let+ _ = next in
             let+ e = Lazy.force expr in
             let+ token = peek in
             begin
               match token with
               | None -> return @@ Error "token exhausted"
               | Some t ->
                 match t with
                 | RParen _ -> let+ _ = next in return e
                 | _ -> return @@ Error "unexpected token"
             end
           | _ -> return @@ Error "unexpected token")

(* mul = primary *)
and mul = lazy (Lazy.force primary)

(* expr  = mul ("+" mul | "-" mul)* *)
and expr =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               let+ _ = next in
               let+ right = Lazy.force mul in
               let n = Result.(let* lnode = left in
                               let* rnode = right in
                               return @@ BinaryOp (i, op, lnode, rnode)) in
               star n
             | _ -> return left) in
  lazy State.(let+ left =  Lazy.force mul in
              star left)

(* program = expr *)
and program = lazy (Lazy.force expr)

let parse = Lazy.force program

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
  |> Result.fold ~ok:(fun s -> Printf.printf "%s\n" s;0)
    ~error:(fun s -> Printf.eprintf "%s\n" s ;1)
  |> exit
