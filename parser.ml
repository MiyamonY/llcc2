open Batteries
open ParserCo
open ParserCo.Infix

module Local = struct
  let local = ref []

  let assign_size () = 8 * (List.length !local)

  let find name = List.assoc_opt name !local

  let add_varaible name =
    local := (name, 8 * (List.length !local)) :: !local
end

type pos = CharParser.position state

type node =
  | Number of pos * int
  | BinaryOp of pos * Operator.t * node * node
  | Variable of pos * string
  | Return of pos * node
  | If of pos * node * node * node option
  | While of pos * node * node

let at = function
  | Number (p, _) -> p
  | BinaryOp (p, _, _, _) -> p
  | Variable (p, _) -> p
  | Return (p, _) -> p
  | If (p, _, _, _) -> p
  | While (p, _, _) -> p

let rec to_string = function
  | Number(_, n)  -> Printf.sprintf "Number(%d)" n
  | BinaryOp(_, op, l, r) ->
    let sl = to_string l in
    let sr = to_string r in
    Printf.sprintf "BinaryOp(%s,%s,%s)\n" (Operator.to_string op) sl sr
  | Variable(_, name) ->
    Printf.sprintf "Variable(%s)" name
  | Return(_, node) ->
    Printf.sprintf "Return\t%s" @@ to_string node
  | If(_, cond, then_, else_) ->
    let el = match else_ with
      | None -> ""
      | Some node -> to_string node in
    Printf.sprintf "If(%s) %s %s" (to_string cond) (to_string then_) el
  | While(_, cond, body) ->
    Printf.sprintf "While(%s) %s" (to_string cond) @@ to_string body

let (let>) = (>>=)

let alpha =
  let> b = one_plus @@ satisfy (fun c -> Char.is_uppercase c || Char.is_lowercase c) in
  return @@ String.of_list b

let digit =
  let> b = one_plus @@ satisfy Char.is_digit in
  return @@ String.of_list b

let alnum = digit <|> alpha

let spaces =
  let space = exactly ' ' in
  ignore_zero_plus space

let ignore_spaces_after m =
  let> node = m in
  let> () = spaces in return node

let ignore_spaces_before m =
  let> () = spaces in
  let> node = m in
  return node

let ignore_spaces m =
  let> () = spaces in
  ignore_spaces_after m

let unary_op =
  let open Operator in
  let plus = exactly '+' >>> return Plus in
  let minus = exactly '-' >>> return Minus in
  plus <|> minus

let mul_op =
  let open Operator in
  let mul = exactly '*' >>> return Mul in
  let div = exactly '/' >>> return Div in
  mul <|> div

let add_op =
  let open Operator in
  let add = exactly '+' >>> return Plus in
  let sub = exactly '-' >>> return Minus in
  add <|> sub

let relational_op =
  let open Operator in
  let lt = exactly '<' >>> return Lt in
  let le = exactly '<' >>> exactly '=' >>> return Le in
  let gt = exactly '>' >>> return Gt in
  let ge = exactly '>' >>> exactly '=' >>> return Ge in
  either [le; lt; ge; gt]

let equality_op =
  let open Operator in
  let eq = exactly '=' >>> exactly '=' >>> return Eq in
  let neq = exactly '!' >>> exactly '=' >>> return Neq in
  eq <|> neq

let variables =
  let> s = state in
  let> name = alpha <|> digit in
  begin match Local.find name with
    | None -> Local.add_varaible name
    | Some _ -> ()
  end;
  let keywords = ["if"; "return"; "while"; "else"] in
  if List.exists ((=) name) keywords then fail
  else return @@ Variable (s, name)

let number =
  let> s = state in
  let> b = one_plus @@ satisfy Char.is_digit in
  return @@ Number (s, String.to_int @@ String.of_list b)

let keyword k =
  String.enum k
  |> Enum.map (fun c -> exactly c)
  |> Enum.fold (fun a b -> a >>> b) (return 'x')

let lparen = exactly '('
let rparen = exactly ')'
let seperator = exactly ';'

(* primary = number | varriable | "(" expr ")" *)
let rec primary = lazy
  (let parens =
     let> _ = lparen in
     let> u = ignore_spaces @@ must @@ Lazy.force expr in
     let> _ = must rparen in return u
   in
   label "primary" @@ either [number; variables; parens])

(* unary = ("+" | "-")? primary *)
and unary = lazy
  (let> s = state in
   let> sign = ignore_spaces @@ maybe unary_op in
   let> p = Lazy.force primary in
   match sign with
   | None -> return p
   | Some op ->
     match op with
     | Plus -> return p
     | Minus -> return @@ BinaryOp(s, Minus, Number(s, 0), p)
     | _ -> fail)

(* mul = unary ("*" unary | "/" unary)* *)
and mul = lazy
  (label "mul" @@
   let rec aux left =
     let> s = state in
     let> op = ignore_spaces @@ maybe mul_op in
     match op with
     | None -> return left
     | Some op ->
       let> right = must @@ Lazy.force unary in
       aux @@ BinaryOp (s, op, left, right) in
   let> u = Lazy.force unary in
   aux u)

(* add  = mul ("+" mul | "-" mul)* *)
and add = lazy
  (label "add" @@
   let rec aux left =
     let> s = state in
     let> op = ignore_spaces @@ maybe add_op in
     match op with
     | None -> return left
     | Some op ->
       let> right = must @@ Lazy.force mul in
       aux @@ BinaryOp (s, op, left, right) in
   let> u = Lazy.force mul in
   aux u)

(* relational = add ("<" add | "<=" add | ">" add | ">=" add) * *)
and relational = lazy
  (label "relational" @@
   let rec aux left =
     let> s = state in
     let> op = ignore_spaces @@ maybe relational_op in
     match op with
     | None -> return left
     | Some op ->
       let> right = Lazy.force add in
       aux (match op with
           | Gt -> BinaryOp (s, Lt, right, left)
           | Ge -> BinaryOp (s, Le, right, left)
           | _ -> BinaryOp (s, op, left, right)) in
   let> u = Lazy.force add in
   aux u)

(* equality = relational ("==" relational | "!=" relational) * *)
and equality = lazy
  (label "equality" @@
   let aux left =
     let> s = state in
     let> op = ignore_spaces @@ maybe equality_op in
     match op with
     | None -> return left
     | Some op ->
       let> right = must @@ Lazy.force relational in
       return @@ BinaryOp (s, op, left, right) in
   let> u = Lazy.force relational in
   aux u)

(* assign = equality ("=" assign)? *)
and assign = lazy
  (label "assign" @@
   let aux left =
     let> s = state in
     let> op = ignore_spaces @@ maybe @@ exactly '=' in
     match op with
     | None -> return left
     | Some _ ->
       let> right = must @@ Lazy.force assign in
       return @@ BinaryOp (s, Assign, left, right) in
   let> u = Lazy.force equality in
   aux u)

(* expr = equality *)
and expr = lazy
  (label "expr" @@
   Lazy.force assign)

(* stmt = expr ";"
        | "return" expr ";"
        | "if" "(" expr ")" stmt ("else" stmt)?
        | "while" "(" expr ")" stmt *)
and stmt = lazy
  (let> s = state in
   let sexpr =
     let> node = ignore_spaces_before @@ Lazy.force expr in
     must @@ ignore_spaces_before seperator >>> return node in
   let sreturn =
     ignore_spaces @@ keyword "return" >>>
     let> node = must @@ ignore_spaces @@ Lazy.force expr in
     must @@ ignore_spaces_before seperator >>> return @@ Return (s, node) in
   let sif =
     ignore_spaces_before @@ keyword "if" >>>
     must @@ ignore_spaces lparen >>>
     let> cond = must @@ Lazy.force expr in
     must @@ ignore_spaces rparen >>>
     let> then_ = must @@ Lazy.force stmt in
     let> else_clause = maybe @@ ignore_spaces @@ keyword "else" in
     match else_clause with
     | None -> return @@ If(s, cond, then_, None)
     | Some _ ->
       let> else_ = must @@ Lazy.force stmt in
       return @@ If(s, cond, then_, Some else_) in
   let swhile =
     ignore_spaces_before @@ keyword "while" >>>
     must @@ ignore_spaces lparen >>>
     let> cond = must @@ Lazy.force expr in
     must @@ ignore_spaces rparen >>>
     let> body = must @@ Lazy.force stmt in
     return @@ While(s, cond, body)
   in
   either [sreturn; sif; swhile; sexpr;])

(* program = stmt *)
and program = lazy
  (let> node = one_plus @@ Lazy.force stmt in
   let> () = eof in
   return node)

let parse ?(debug=false) input =
  debug_mode := debug;
  run (Lazy.force program) (CharParser.source_of_string input)
