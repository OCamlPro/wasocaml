external saucisse : int -> unit = "saucisse"

external ( + ) : int -> int -> int = "%addint"

let test b o p =
  let[@local] f x y =
    x + y
  in
  if b then
    f 1 o
  else
    f 2 p
[@@inline never]

let () =
  saucisse (test false 1 2);
  saucisse (test true 1 2)
