include BatResult

let (let*) m k =
  match m with
  | Ok x -> k x
  | Error _  as e ->  e

let return x = Ok x
