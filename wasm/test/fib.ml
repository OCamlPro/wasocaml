let rec fib n =  if n < 2 then 1 else fib (n - 1) + fib (n - 2);;

let () =
  for i = 0 to 40 do
    print_string "fib (";
    print_int i;
    print_string ") = ";
    print_int (fib i);
    print_string "\n"
  done
