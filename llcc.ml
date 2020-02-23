open Batteries

let rec parse_int n = function
  | [] -> (n, [])
  | c::rest ->
    match c with
    |  '0' .. '9' ->
      let m = (Char.code c -  Char.code '0') in
      parse_int (n*10 + m) rest
    | _ -> (n, c::rest)


let rec parse = function
  | [] -> []
  | c::rest ->
    match c with
    | '+' ->
      let (n, r) = parse_int 0 rest in
      (Printf.sprintf "add rax, %d" n) :: parse r
    | '-' ->
      let (n, r) = parse_int 0 rest in
      (Printf.sprintf "sub rax, %d" n):: parse r
    | _ -> []

let () =
  Printf.printf "%s\n"
    (if Array.length Sys.argv != 2 then
       "引数の個数が正しくありません"
     else
       let s = Sys.argv.(1) in
       let (n, r) = parse_int 0 @@ String.to_list s in
       let parsed = parse r in
       String.concat "\n" @@ [".intel_syntax noprefix";
                              ".global main";
                              "main:";
                              Printf.sprintf "\tmov rax, %d" n]
                             @ parsed @ ["\tret"])
