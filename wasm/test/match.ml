external saucisse : int -> unit = "saucisse"

external ( + ) : int -> int -> int = "%addint"

type 'a option =
  | None
  | Some of 'a

let get_def ~def opt = match opt with None -> def | Some v -> v
  [@@inline never]

let () =
  saucisse (get_def ~def:3 None);
  saucisse (get_def ~def:3 (Some 42))

module List = struct
  let rec length_aux len = function
    | [] -> len
    | _ :: l -> length_aux (len + 1) l

  let length l = length_aux 0 l

  let rec map f = function
    | [] -> []
    | a :: l ->
      let r = f a in
      r :: map f l

  let rec fold_left f accu l =
    match l with [] -> accu | a :: l -> fold_left f (f accu a) l
end

let succ x = x + 1

let lst = [1;2;3;4;5]

let sum l = List.fold_left (+) 0 l

let () = saucisse (List.length lst)
let () = saucisse (sum lst)

let lst' = List.map succ lst

let () = saucisse (sum lst')
