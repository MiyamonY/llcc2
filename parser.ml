open Batteries
open Tokenizer

module Result = Mresult

type node =
  | Number of pos * int
  | BinaryOp of pos * Operator.t * node * node

let rec print_node = function
  | Number(_, n)  -> Printf.sprintf "Number(%d)" n
  | BinaryOp(_, op, l, r) ->
    let sl = print_node l in
    let sr = print_node r in
    Printf.sprintf "BinaryOp(%s,%s,%s)\n" (Operator.to_string op) sl sr

let peek =
  State.(let+ tokens = get in
         match tokens with
         | [] -> return None
         | t:: _ -> return @@ Some t)

let next =
  State.(let+ tokens = get in
         match tokens with
         | [] -> return @@ Result.error @@ `ParserError (-1, "token exhausted")
         | _::rest ->
           let+ () = put rest in return Result.(return ()))

(* primary = num | "(" expr ")" *)
let rec primary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ Result.error @@ `ParserError (-1, "token exhausted")
         | Some token ->
           match token with
           | Num (i, n) ->
             let+ _ = next in
             return Result.(return @@ Number (i, n))
           | LParen _ ->
             let+ _ = next in
             let+ e = Lazy.force expr in
             let+ token = peek in
             begin
               match token with
               | None -> return @@ Result.error @@ `ParserError (-1, "token exhausted")
               | Some t ->
                 match t with
                 | RParen _ -> let+ _ = next in return e
                 | _   -> return @@ Result.error @@
                   `ParserError (Tokenizer.at t, Printf.sprintf "unexpected token: %s" @@ Tokenizer.to_string t)
             end
           | _ -> return @@ Result.error @@
             `ParserError (Tokenizer.at token, Printf.sprintf "unexpected token: %s" @@ Tokenizer.to_string token))

(* unary = ("+" | "-")? primary *)
and unary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ Result.error @@ `ParserError (-1, "token exhausted")
         | Some token ->
           match token with
           | Reserved (i, op) ->
             begin match op with
               | Plus ->
                 let+ _ = next in
                 Lazy.force primary
               | Minus ->
                 let+ _ = next in
                 let+ node = Lazy.force primary in
                 return Result.(
                     let* n = node in
                     return @@ BinaryOp(i, Minus, Number (i, 0), n))
               | _ -> Lazy.force primary
             end
           | _ -> Lazy.force primary)

(* mul = unary ("*" unary | "/" unary)* *)
and mul =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Mul | Div ->
                   let+ _ = next in
                   let+ right = Lazy.force unary in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return @@ BinaryOp (i, op, lnode, rnode)) in
                   star n
                 | _ -> return left
               end
             | _ -> return left) in
  lazy State.(let+ left =  Lazy.force unary in
              star left)

(* add  = mul ("+" mul | "-" mul)* *)
and add =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Plus | Minus ->
                   let+ _ = next in
                   let+ right = Lazy.force mul in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return @@ BinaryOp (i, op, lnode, rnode)) in
                   star n
                 | _ -> return left
               end
             | _ -> return left) in
  lazy State.(let+ left = Lazy.force mul in
              star left)

(* relational = add ("<" add | "<=" add | ">" add | ">=" add) * *)
and relational =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Lt | Le | Gt | Ge ->
                   let+ _ = next in
                   let+ right = Lazy.force add in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return (
                                     if op = Gt then
                                       BinaryOp (i, Lt, rnode, lnode)
                                     else if op = Ge then
                                       BinaryOp (i, Le, rnode, lnode)
                                     else
                                       BinaryOp (i, op, lnode, rnode))) in
                   star n
                 | _ -> return left
               end
             | _ -> return left) in

  lazy State.(let+ left = Lazy.force add in
              star left)

(* equality = relational ("==" relational | "!=" relational) * *)
and equality =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Eq | Neq ->
                   let+ _ = next in
                   let+ right = Lazy.force relational in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return @@ BinaryOp (i, op, lnode, rnode)) in
                   star n
                 | _ ->
                   return left
               end
             | _ -> return left) in
  lazy State.(let+ left = Lazy.force relational in
              star left)

(*  expr = equality *)
and expr = lazy
  (Lazy.force equality)

(* program = expr *)
and program = lazy (Lazy.force expr)

let parse =
  Lazy.force program