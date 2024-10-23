type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec aux p = function
  | Nil -> Cons (1, Nil)
  | Cons (hd, tl) -> Cons (p + hd, aux hd tl)

let next = function Cons (1, tl) -> Cons (1, aux 1 tl) | _ -> assert false

let rec pascal n = if n = 0 then Cons (1, Nil) else next (pascal (n - 1))

let rec print = function
  | Nil -> print_string "\n"
  | Cons (hd, tl) ->
    print_int hd;
    print_string " ;";
    print tl

let () =
  for i = 0 to 400000 do
    print (pascal 32)
  done
