(* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Troestler Christophe
 * Modified by Fabrice Le Fessant
*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let rec make i d =
  if d = 0
  then Node (Empty, i, Empty)
  else
    let i2 = 2 * i in
    let d = d - 1 in
    let l = make (i2 - 1) in
    let r = make i2 d in
    let l = l d in
    Node (l, i, r)

let rec check = function
  | Empty -> 0
  | Node (l, i, r) ->
    let l = check l in
    let r = check r in
    i + l + r

let min_depth = 4

let max_depth =
  let n = 10 in
  max (min_depth + 2) n

let long_lived_tree = make 0 max_depth

let loop_depths d =
  for i = 0 to ((max_depth - d) / 2) + 1 - 1 do
    let d = d + (i * 2) in
    let r_lsl = max_depth - d + min_depth in
    let niter = 1 lsl r_lsl in
    let c = ref 0 in
    for i = 1 to niter do
      let a = check (make i d) in
      let b = check (make (-i) d) in
      c := !c + a + b;
    done;
    print_int (2 * niter);
    print_string " trees of depth ";
    print_int d;
    print_string " check ";
    print_int !c;
    print_string "\n"
  done

let () =
  loop_depths min_depth
