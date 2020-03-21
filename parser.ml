open Batteries
open ParserCo
open ParserCo.Infix

module Local :
sig
  type name = string
  type t
  val create : unit -> t
  val assign_size: t -> int
  val find: t -> name -> int option
  val add_variable: t -> name -> unit
  val list_all_variables: t -> name list
end = struct
  type name = string
  type t  = (name * int) list ref
  let create () = ref []

  let assign_size local =
    let n = 8 * (List.length !local) in
    if n mod 16 = 0 then n
    else n + 8

  let find local name = List.assoc_opt name !local

  let add_variable local name =
    local := (name, 8 * (List.length !local)) :: !local

  let list_all_variables local =
    List.map Tuple2.first !local
end

type pos = CharParser.position state

type args = string list

type node =
  | Number of pos * int
  | BinaryOp of pos * Operator.t * node * node
  | Variable of pos * string
  | Return of pos * node
  | If of pos * node * node * node option
  | While of pos * node * node
  | For of pos * node option * node option * node option * node
  | Block of pos * node list
  | FuncCall of pos * string * node list
  | FuncDecl of pos * string * Local.t * args * node list

let at = function
  | Number (p, _) -> p
  | BinaryOp (p, _, _, _) -> p
  | Variable (p, _) -> p
  | Return (p, _) -> p
  | If (p, _, _, _) -> p
  | While (p, _, _) -> p
  | For (p, _, _ , _, _) -> p
  | Block (p, _) -> p
  | FuncCall (p, _, _) -> p
  | FuncDecl (p, _, _, _, _) -> p

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
  | For(_, _, _ , _, body) ->
    Printf.sprintf "For(;;) %s" (to_string body)
  | Block(_, stmts) ->
    String.concat "\n" @@ List.map to_string stmts
  | FuncCall (_, name, args) ->
    Printf.sprintf "Call %s(%s)" name @@ String.concat ", " @@ List.map to_string args
  | FuncDecl (_, name, _, args, body) ->
    Printf.sprintf "FuncDecl %s(%s){%s}" name
      (String.concat ", " args)
    @@ String.concat "\n" @@ List.map to_string body

let (let>) = (>>=)

let alpha =
  let> b = one_plus @@ satisfy (fun c -> Char.is_uppercase c || Char.is_lowercase c) in
  return @@ String.of_list b

let digit =
  let> b = one_plus @@ satisfy Char.is_digit in
  return @@ String.of_list b

let alnum = digit <|> alpha

let identifier =
  let> first = alpha in
  let> rest  = zero_plus @@ either [alpha; digit] in
  let name = first ^ String.concat "" rest in
  let keywords = ["if"; "return"; "while"; "else"; "for"] in
  if List.exists ((=) name) keywords then fail
  else return @@ name

let spaces =
  let space = either [exactly ' '; exactly '\n'] in
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
let lcbrace = exactly '{'
let rcbrace = exactly '}'
let seperator = exactly ';'

(* primary = number | ident("(" (expr ("," expr)* )?  ")")? | "(" expr ")" *)
let rec primary local =
  let parens =
    let> _ = lparen in
    let> u = ignore_spaces @@ must @@ expr local in
    let> _ = must rparen in return u in
  let ident =
    let> s = state in
    let> ident = label "identifier" @@ identifier in
    let> next = label "lparen" @@ maybe lparen in
    match next with
    | None ->
      Local.add_variable local ident;
      return @@ Variable(s, ident)
    | Some _ ->
      let> args = label "arguemnts" @@
        zero_plus ~sep: (ignore_spaces @@ exactly ',') (expr local) in
      label "rparen" @@ must rparen >>> return @@ FuncCall(s, ident, args)
  in
  label "primary" @@ either [number; ident; parens]

(* unary = ("+" | "-")? primary *)
and unary local =
  let> s = state in
  let> sign = ignore_spaces @@ maybe unary_op in
  let> p = primary local in
  match sign with
  | None -> return p
  | Some op ->
    match op with
    | Plus -> return p
    | Minus -> return @@ BinaryOp(s, Minus, Number(s, 0), p)
    | _ -> fail

(* mul = unary ("*" unary | "/" unary)* *)
and mul local = lazy
  (label "mul" @@
   let rec aux left =
     let> s = state in
     let> op = ignore_spaces @@ maybe mul_op in
     match op with
     | None -> return left
     | Some op ->
       let> right = must @@ unary local in
       aux @@ BinaryOp (s, op, left, right) in
   let> u =  unary local in
   aux u)

(* add  = mul ("+" mul | "-" mul)* *)
and add local =
  label "add" @@
  let rec aux left =
    let> s = state in
    let> op = ignore_spaces @@ maybe add_op in
    match op with
    | None -> return left
    | Some op ->
      let> right = must @@ Lazy.force @@ mul local in
      aux @@ BinaryOp (s, op, left, right) in
  let> u = Lazy.force @@ mul local in
  aux u

(* relational = add ("<" add | "<=" add | ">" add | ">=" add) * *)
and relational local =
  label "relational" @@
  let rec aux left =
    let> s = state in
    let> op = ignore_spaces @@ maybe relational_op in
    match op with
    | None -> return left
    | Some op ->
      let> right = add local in
      aux (match op with
          | Gt -> BinaryOp (s, Lt, right, left)
          | Ge -> BinaryOp (s, Le, right, left)
          | _ -> BinaryOp (s, op, left, right)) in
  let> u = add local in
  aux u

(* equality = relational ("==" relational | "!=" relational) * *)
and equality local =
  label "equality" @@
  let aux left =
    let> s = state in
    let> op = ignore_spaces @@ maybe equality_op in
    match op with
    | None -> return left
    | Some op ->
      let> right = must @@ relational local in
      return @@ BinaryOp (s, op, left, right) in
  let> u = relational local in
  aux u

(* assign = equality ("=" assign)? *)
and assign local =
  let aux left =
    let> s = state in
    let> op = ignore_spaces @@ maybe @@ exactly '=' in
    match op with
    | None -> return left
    | Some _ ->
      let> right = must @@ assign local in
      return @@ BinaryOp (s, Assign, left, right) in
  let> u = equality local in
  aux u

(* expr = equality *)
and expr local = assign local

(* stmt = expr ";"
        | "return" expr ";"
        | "if" "(" expr ")" stmt ("else" stmt)?
        | "while" "(" expr ")" stmt
        | "for" "(" expr? ";" expr? ";" expr? ")" stmt
        | "{" stmt* "}"*)
and stmt local =
  let> s = state in
  let sexpr =
    let> node = ignore_spaces_before @@ expr local in
    must @@ ignore_spaces_before seperator >>> return node in
  let sreturn =
    ignore_spaces @@ keyword "return" >>>
    let> node = must @@ ignore_spaces @@ expr local in
    must @@ ignore_spaces_before seperator >>> return @@ Return (s, node) in
  let sif =
    ignore_spaces_before @@ keyword "if" >>>
    must @@ ignore_spaces lparen >>>
    let> cond = must @@ expr local in
    must @@ ignore_spaces rparen >>>
    let> then_ = must @@ stmt local in
    let> else_clause = maybe @@ ignore_spaces @@ keyword "else" in
    match else_clause with
    | None -> return @@ If(s, cond, then_, None)
    | Some _ ->
      let> else_ = must @@ stmt local in
      return @@ If(s, cond, then_, Some else_) in
  let swhile =
    ignore_spaces_before @@ keyword "while" >>>
    must @@ ignore_spaces lparen >>>
    let> cond = must @@ expr local in
    must @@ ignore_spaces rparen >>>
    let> body = must @@ stmt local in
    return @@ While(s, cond, body) in
  let sfor =
    label "for" @@ ignore_spaces_before @@ keyword "for" >>>
    must @@ ignore_spaces lparen >>>
    let> init = maybe @@ expr local in
    must @@ ignore_spaces seperator >>>
    let> cond = maybe @@ expr local in
    must @@ ignore_spaces seperator >>>
    let> next = maybe @@ expr local in
    must @@ ignore_spaces rparen >>>
    let> body = must @@ stmt local in
    return @@ For(s, init, cond, next, body) in
  let sblock =
    label "block" @@ ignore_spaces @@ lcbrace >>>
    let> stmts = must @@ zero_plus @@ stmt local in
    must @@ ignore_spaces @@ rcbrace >>>
    return @@ Block(s, stmts)
  in
  either [sreturn; sif; swhile; sfor; sblock; sexpr;]

(* decls = (ident "(" ( expr ("," expr)* )? ")" "{" stmts* "}")* *)
and decls = lazy
  (let func_decl = let> s = state in
     let local = Local.create () in
     let> ident = identifier in
     must @@ ignore_spaces lparen >>>
     let> args = zero_plus ~sep:(ignore_spaces @@ exactly ',') identifier in
     List.iter (Local.add_variable local) args;
     must @@ ignore_spaces rparen >>>
     must @@ ignore_spaces lcbrace >>>
     let> body = zero_plus @@ stmt local in
     ignore_spaces @@ must rcbrace >>>
     return @@ FuncDecl (s, ident, local, args, body)in
   one_plus func_decl)

(* program = decls *)
and program = lazy
  (let> node = Lazy.force decls in
   let> () = label "eof" @@ eof in
   return node)

let parse ?(debug=false) input =
  debug_mode := debug;
  run (Lazy.force program) @@ CharParser.source_of_string input
