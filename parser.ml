open Batteries
open Tokenizer

module Result = Mresult

type node =
  | Number of pos * int
  | BinaryOp of pos * Operator.t * node * node
  | Variable of pos * string

type program = node list

let local = ref []

let local_assign_size local = 8 * (List.length !local)

let add_varaible local name =
  local := (name, 8 * (List.length !local)) :: !local

let at = function
  | Number (p, _) -> p
  | BinaryOp (p, _, _, _) -> p
  | Variable (p, _) -> p

let rec print_node = function
  | Number(_, n)  -> Printf.sprintf "Number(%d)" n
  | BinaryOp(_, op, l, r) ->
    let sl = print_node l in
    let sr = print_node r in
    Printf.sprintf "BinaryOp(%s,%s,%s)\n" (Operator.to_string op) sl sr
  | Variable(_, name) ->
    Printf.sprintf "Variable(%s)" name

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

(* primary = num | var | "(" expr ")" *)
let rec primary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ Result.error @@ `ParserError (-1, "token exhausted")
         | Some token ->
           match token with
           | Num (i, n) ->
             let+ _ = next in
             return Result.(return @@ Number (i, n))
           | Var (i, name) ->
             let+ _ = next in
             begin match List.assoc_opt name !local with
               | None -> add_varaible local name
               | Some _ -> ()
             end;
             return Result.(return @@ Variable(i, name))
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

(* assign = equality ("=" assign)? *)
and assign =
  let rec star left =
    State.(let+ token = peek in
           match token with
           | None -> return left
           | Some t ->
             match t with
             | Reserved (i, op) ->
               begin match op with
                 | Assign ->
                   let+ _ = next in
                   let+ right = Lazy.force assign in
                   let n = Result.(let* lnode = left in
                                   let* rnode = right in
                                   return @@ BinaryOp (i, op, lnode, rnode)) in
                   star n
                 | _ -> return left
               end
             | _ -> return left
          ) in
  lazy State.(let+ left = Lazy.force equality in
              star left)

(*  expr = equality *)
and expr = lazy
  (Lazy.force assign)

(* stmt = expr ";" *)
and stmt  = lazy
  State.(let+ st = Lazy.force expr in
         let+ token = peek in
         match token with
         | None ->
           return Result.(error @@ `ParserError (-1, "token exhausted"))
         | Some t ->
           match t with
           | Sep _ ->
             let+ _ = next in
             return @@ st
           | _ ->
             return Result.(error @@ `ParserError (Tokenizer.at t, Printf.sprintf "unexpected token: %s" @@ Tokenizer.to_string t)))

(* program = stmt* *)
and program = lazy
  State.(let+ token = peek in
         match token with
         | None -> return Result.(return [])
         | Some _ ->
           let+ st = Lazy.force stmt in
           match st with (* TODO: improve code *)
           | Result.Ok _ ->
             let+ sts = Lazy.force program in
             return Result.(let* s = st in
                            let* ss = sts in
                            return @@ s::ss)
           | Result.Error _ ->
             return Result.(let* s = st in return [s]))

let parse =
  Lazy.force program
