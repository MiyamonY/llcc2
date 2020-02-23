open Batteries

let () =
  Printf.printf "%s\n"
    (if Array.length Sys.argv != 2 then
       "引数の個数が正しくありません"
     else
       let n = Sys.argv.(1) in
       String.concat "\n" [".intel_syntax noprefix";
                           ".global main";
                           "main:";
                           Printf.sprintf "\tmov rax, %s" n;
                           "\tret";])
