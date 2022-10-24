external ( + ) : int -> int -> int = "%addint"

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec aux p = function
  | Nil -> Cons (1, Nil)
  | Cons (hd, tl) -> Cons (p + hd, aux hd tl)

let next = function Cons (1, tl) -> Cons (1, aux 1 tl) | _ -> assert false

let rec pascal n = if n = 0 then Cons (1, Nil) else next (pascal (n - 1))

let rec print = function
  | Nil -> Format.printf "@."
  | Cons (hd, tl) ->
    Format.printf "%d ;" hd;
    print tl

let () = print (pascal 12)
