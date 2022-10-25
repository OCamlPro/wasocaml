external saucisse : int -> unit = "saucisse"

external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( * ) : int -> int -> int = "%mulint"

(*
external ( < ) : 'a -> 'a -> bool = "%lessthan"
*)

let f x y z = x + y - (z * x)

let g x y = (f [@inlined never]) x x y

let[@inlined never] foo = (f [@inlined never]) 42

let h x = saucisse x

let () = h ((foo [@inlined never]) 1 2)

let[@inlined never] opaque_identity (x : bool) : bool = x

let left x y = if (opaque_identity [@inlined never]) false then y else x

let right x y = if (opaque_identity [@inlined never]) false then x else y

let left' x y = if (opaque_identity [@inlined never]) true then x else y

let right' x y = if (opaque_identity [@inlined never]) true then y else x

let () =
  h (left 1 2);
  h (right 1 2);
  h (left' 3 4);
  h (right' 3 4)

(*
let rec fibo x =
  (*
  if x < 0 then failwith "fibo"
  else
  *)
  if x < 2 then x
  else fibo (x - 1) + fibo (x - 2)

let () =
  h (fibo 10)
  *)
