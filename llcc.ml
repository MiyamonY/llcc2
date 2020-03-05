open Batteries

module Result = Mresult

type pos = int

let input = ref ""

let sep pos =
  (match pos with
   | None -> String.length !input
   | Some i ->  i)
  |> String.repeat " "

let error_message  = function
  | `ArgumentError msg ->
    Printf.sprintf "Argument error: %s" msg
  | `TokenizerError (pos, msg) ->
    Printf.sprintf "Tokenizer error:\n%s\n%s^ %s" !input (sep pos) msg
  | `ParserError (pos, msg) ->
    Printf.sprintf "Parser error:\n%s\n%s^ %s" !input (sep pos) msg
  | `GeneratorError (pos, msg) ->
    Printf.sprintf "Generator error:\n%s\n%s^ %s" !input (sep pos) msg

let () =
  (if Array.length Sys.argv != 2 then
     Result.error (`ArgumentError "The number of arugument is 1")
   else
     begin
       input := Sys.argv.(1);
       Result.(let* tokens = Tokenizer.tokenize !input in
               let (parsed, _) = State.runState Parser.parse tokens in
               let* p = parsed in
               Generator.generate p)
     end)
  |> Result.fold
    ~ok:(fun s -> Printf.printf "%s\n" s;0)
    ~error:(fun s -> Printf.eprintf "%s\n" @@ error_message s ;1)
  |> exit
