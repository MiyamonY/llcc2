open Batteries

type pos = int

let debug = ref false
let input = ref ""

let sep pos = String.repeat " " (pos |? String.length !input)

let error_message  = function
  | `ArgumentError msg ->
    Printf.sprintf "Argument error: %s" msg
  | `ParserError (ParserCo.Report reps) ->
    (match reps with
     | [] -> "Parser error"
     | (state, msg, _)::_ ->
       Printf.sprintf "Parser error: %s\n%s" msg
         (match state with
          | Eof -> "@eof"
          | State {CharParser.offset = o; _} ->
            Printf.sprintf "%s\n%s^" !input @@ sep (Some o)))
  | `GeneratorError (_, msg) ->
    Printf.sprintf "Generator error:\n%s\n %s" !input msg

let (let*) = Result.Monad.bind

let () =
  (if Array.length Sys.argv != 2 then
     Result.error (`ArgumentError "The number of arugument is 1")
   else
     begin
       input := Sys.argv.(1);
       let* parsed =
         Parser.parse ~debug:!debug !input |> Result.map_error (fun err -> `ParserError err) in
       if !debug then Printf.eprintf "%s" @@ String.concat "\n" @@ List.map Parser.to_string parsed;
       Generator.generate parsed
     end)
  |> Result.fold
    ~ok:(fun s -> Printf.printf "%s\n" s;0)
    ~error:(fun s -> Printf.eprintf "%s\n" @@ error_message s;1)
  |> exit
