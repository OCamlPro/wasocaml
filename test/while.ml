external saucisse : int -> unit = "saucisse"
let[@inlined never] opaque_identity (x : bool) : bool = x

external ( + ) : int -> int -> int = "%addint"

let loop b =
  let r = ref b in
  let count = ref 0 in
  while !r do
    incr count;
    r := false
  done;
  !count
[@@inline never]

let () =
  saucisse (loop false);
  saucisse (loop true)
