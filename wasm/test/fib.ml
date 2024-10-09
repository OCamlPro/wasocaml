let rec fib n =
  if n < 2 then n
  else fib (n - 1) + fib (n - 2)

let () =
  let n = 40 in
  assert (fib n = 102334155)
  (*
  for i = 0 to 40 do
    print_string "fib (";
    print_int i;
    print_string ") = ";
    print_int (fib i);
    print_string "\n"
  done
  *)
