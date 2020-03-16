open Batteries

type (+'a, 'b) t = ('a * 'b list)

let return a = (a, [])
let bind (a, s) f = let (b, t) = f a in (b, s @ t)

let tell s =  (Ok (), s)

let run (result, b) =
  match result with
  | Ok _ -> Ok b
  | Error err -> Error err

module Infix = struct
  let (let@) = bind
  let (>>>) x f = bind x (fun _ -> f )
end
