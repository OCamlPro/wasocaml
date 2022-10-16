external saucisse : int -> unit = "saucisse"
external morteau : float -> unit = "morteau"
external montbeliard : (float [@unboxed]) -> unit = "montbeliard_byte" "montbeliard"

external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"

external opaque_identity : 'a -> 'a = "%opaque"

let f x y z =
  x + y - z * x

let g x y =
  (f [@inlined never]) x x y

let h x =
  saucisse x

let i x =
  x, x

let j x =
  let (a, _) = x in
  a

let s () =
  "plop"

let () =
  h 3

let rec stuff x = stuff x

type a = A of (unit -> a)
let rec chose () = A chose

let v1 = 12.34
let v2 = 12l
let v3 = 12L
let v4 = 12n

let uuu x y z =
  x +. y *. x /. y -. z

type plop = { a : int; b : plop; c : int }
let rec a = { a = 1; b; c = 2 }
and b = { a = 10; b = a; c = 20 }

let () =
  h (opaque_identity 5);
  morteau (uuu 12.34 456. 2.);
  montbeliard 78.
