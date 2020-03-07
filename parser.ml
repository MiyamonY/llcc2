open Batteries
open Tokenizer

module Result = Mresult

type node =
  | Number of pos * int
  | BinaryOp of pos * Operator.t * node * node
  | Variable of pos * string
  | Return of pos * node
  | If of pos * node * node * node option
  | While of pos * node * node

type program = node list

module Local = struct
  let local = ref []

  let assign_size () = 8 * (List.length !local)

  let find name = List.assoc_opt name !local

  let add_varaible name =
    local := (name, 8 * (List.length !local)) :: !local
end

let at = function
  | Number (p, _) -> p
  | BinaryOp (p, _, _, _) -> p
  | Variable (p, _) -> p
  | Return (p, _) -> p
  | If (p, _, _, _) -> p
  | While (p, _, _) -> p

let rec print_node = function
  | Number(_, n)  -> Printf.sprintf "Number(%d)" n
  | BinaryOp(_, op, l, r) ->
    let sl = print_node l in
    let sr = print_node r in
    Printf.sprintf "BinaryOp(%s,%s,%s)\n" (Operator.to_string op) sl sr
  | Variable(_, name) ->
    Printf.sprintf "Variable(%s)" name
  | Return(_, node) ->
    Printf.sprintf "Return\t%s" @@ print_node node
  | If(_, cond, then_, else_) ->
    let el = match else_ with
      | None -> ""
      | Some node -> print_node node in
    Printf.sprintf "If(%s) %s %s" (print_node cond) (print_node then_) el
  | While(_, cond, body) ->
    Printf.sprintf "While(%s) %s" (print_node cond) @@ print_node body

let token_exhausted loc =
  Result.error @@ `ParserError (None, Printf.sprintf "[%s]token exhausted" loc)

let unexpected_token loc token =
  Result.error @@ `ParserError (Some (Tokenizer.at token),
                                Printf.sprintf "[%s]unexpected token: %s" loc @@ Tokenizer.to_string token)

let peek =
  State.(let+ tokens = get in
         match tokens with
         | [] -> return None
         | t:: _ -> return @@ Some t)

let next =
  State.(let+ tokens = get in
         match tokens with
         | [] -> return @@ token_exhausted __LOC__
         | _::rest ->
           let+ () = put rest in return Result.(return ()))

let must_be t =
  State.(let+ s = peek in
         match s with
         | None -> return false
         | Some s ->
           begin match t, s with
             | Num _ , Num _ -> return true
             | Reserved _, Reserved _ -> return true
             | LParen _, LParen _ -> return true
             | RParen _, RParen _ -> return true
             | Var _, Var _ -> return true
             | Sep _ , Sep _ -> return true
             | Return _ , Return _ -> return true
             | If _, If _ -> return true
             | Else _, Else _ -> return true
             | _, _ -> return false
           end)

(* primary = num | var | "(" expr ")" *)
let rec primary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ token_exhausted __LOC__
         | Some token ->
           match token with
           | Num (i, n) ->
             let+ _ = next in
             return Result.(return @@ Number (i, n))
           | Var (i, name) ->
             let+ _ = next in
             begin match Local.find name with
               | None -> Local.add_varaible name
               | Some _ -> ()
             end;
             return Result.(return @@ Variable(i, name))
           | LParen _ ->
             let+ _ = next in
             let+ e = Lazy.force expr in
             let+ token = peek in
             begin
               match token with
               | None -> return @@ token_exhausted __LOC__
               | Some t ->
                 match t with
                 | RParen _ -> let+ _ = next in return e
                 | _   -> return @@ unexpected_token __LOC__ t
             end
           | _ -> return @@ unexpected_token __LOC__ token)

(* unary = ("+" | "-")? primary *)
and unary = lazy
  State.(let+ t = peek in
         match t with
         | None -> return @@ token_exhausted __LOC__
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

(* stmt = expr ";"
        | "return" expr ";"
        | "if" "(" expr ")" stmt *)
and stmt  = lazy
  State.(
    let+ token = peek in
    match token with
    | None -> return @@ token_exhausted __LOC__
    | Some t ->
      match t with
      | Return p ->
        let+ _ = next in
        let+ st = Lazy.force expr in
        let+ token = peek in
        begin match token with
          | None ->
            return @@ token_exhausted __LOC__
          | Some t ->
            match t with
            | Sep _ ->
              let+ _ = next in
              return Result.(let* st = st in return @@ Return (p, st))
            | _ -> return @@ unexpected_token __LOC__ t
        end
      | If p ->
        let+ _ = next in
        let+ t = must_be (LParen 0) in
        if t then
          let+ _ = next in
          let+ cond = Lazy.force expr in
          let+ t = must_be (RParen 0) in
          if t then
            let+ _ = next in
            let+ then_ = Lazy.force stmt in
            let+ token = peek in
            match token with
            | None -> return Result.(let* cond = cond in
                                     let* then_ = then_ in
                                     return @@ (If (p, cond, then_, None)))
            | Some token ->
              match token with
              | Else _ ->
                let+ _ = next in
                let+ else_ = Lazy.force stmt in
                return Result.(let* cond = cond in
                               let* then_ = then_ in
                               let* else_ = else_ in
                               return @@ (If (p, cond, then_, Some else_)))
              | _ ->
                return Result.(let* cond = cond in
                               let* then_ = then_ in
                               return @@ (If (p, cond, then_, None)))
          else
            let+ next = peek in
            match next with
            | None -> return @@ token_exhausted __LOC__
            | Some token -> return @@ unexpected_token __LOC__ token
        else
          let+ next = peek in
          begin match next with
            | None -> return @@ token_exhausted __LOC__
            | Some token -> return @@ unexpected_token __LOC__ token
          end
      | While p ->
        let+ _ = next in
        let+ t = must_be (LParen 0) in
        if t then
          let+ _ = next in
          let+ cond = Lazy.force expr in
          let+ t = must_be (RParen 0) in
          if t then
            let+ _ = next in
            let+ body = Lazy.force stmt in
            return Result.(let* cond = cond in
                           let* body = body in
                           return @@ While (p, cond, body))
          else
            let+ next = peek in
            match next with
            | None -> return @@ token_exhausted __LOC__
            | Some token -> return @@ unexpected_token __LOC__ token
        else
          let+ next = peek in
          begin match next with
            | None -> return @@ token_exhausted __LOC__
            | Some token -> return @@ unexpected_token __LOC__ token
          end
      |_ ->
        let+ st = Lazy.force expr in
        let+ token = peek in
        match token with
        | None ->
          return @@ token_exhausted __LOC__
        | Some t ->
          match t with
          | Sep _ ->
            let+ _ = next in
            return @@ st
          | _ -> return @@ unexpected_token __LOC__ t)

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
