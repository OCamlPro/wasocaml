
external saucisse : int -> unit = "saucisse"
external saucissef : float -> unit = "saucissef"

let f x = [| x; x |]
[@@inline never]

let n = (f 1.).(0)

let m = (f 1).(0)

let g x =
  (f x).(0)
[@@inline never]

let () = saucissef n
let () = saucisse m
let () = saucisse (g 2)
let () = saucissef (g 3.)
