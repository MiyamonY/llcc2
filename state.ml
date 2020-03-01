type (+'a, 'b) t = 'b -> ('a*'b)

let return x = fun s -> (x, s)

let bind m f = fun s ->
  let (a, ns) = m s in
  (f a) ns

let (let+) = bind

let execState m = fun s -> let (_, s) = m s in s

let evalState m = fun s -> let (a, _) = m s in a

let runState m = fun s -> m s

let get = fun s -> (s, s)

let put s = fun _ -> ((), s)
