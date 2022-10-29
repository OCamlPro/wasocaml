external saucisse : int -> unit = "saucisse"

external ( + ) : int -> int -> int = "%addint"

external ( - ) : int -> int -> int = "%subint"

external ( * ) : int -> int -> int = "%mulint"

external ( = ) : 'a -> 'a -> bool = "%equal"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"

external opaque_identity : 'a -> 'a = "%opaque"

let () =
  let[@inline never] test (x:int) y =
    saucisse (compare x y)
  in
  saucisse (compare 1 0);
  saucisse (compare 0 1);
  test 1 0;
  test 0 1;
  test 1 1;
  test (-1) (-1);
  ()

external float_of_bits : int64 -> float
  = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

let nan =
  float_of_bits 0x7F_F0_00_00_00_00_00_01L

let () =
  let[@inline never] test (x:float) y =
    saucisse (compare x y)
  in
  saucisse (compare 1. 0.);
  saucisse (compare 0. 1.);
  test 1. 0.;
  test 0. 1.;
  test 1. 1.;
  test (-1.) (-1.);
  test nan nan;
  test nan 0.;
  test 0. nan;
  ()
