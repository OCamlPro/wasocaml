
external saucisse : int -> unit = "saucisse"
let[@inlined never] opaque_identity (x : bool) : bool = x

external ( + ) : int -> int -> int = "%addint"

type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

let up_loop n =
  for i = 2 to n do
    saucisse i
  done
[@@inline never]

let down_loop n =
  for i = n downto 3 do
    saucisse i
  done
[@@inline never]

let () =
  up_loop 5;
  up_loop 2;
  up_loop 1;
  down_loop 5;
  down_loop 3;
  down_loop 1;
  ()
