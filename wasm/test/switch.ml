external saucisse : int -> unit = "saucisse"

external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( * ) : int -> int -> int = "%mulint"

type t =
  | A
  | B
  | C
  | D of int
  | E of int * int
  | F of int

let f x =
  match x with
  | A -> 1
  | B -> 2
  | C -> 3
  | D n -> n
  | E (n, m) -> n + m
  | F n -> n * 2

