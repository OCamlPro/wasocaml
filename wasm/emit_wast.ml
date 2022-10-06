type t =
  | Int of int64
  | Node of
      { name : string;
        args : t list
      }

let print_lst f ppf l =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") f ppf l

let rec emit ppf = function
  | Int i -> Format.fprintf ppf "%Li" i
  | Node { name; args } -> begin
    match args with
    | [] -> Format.pp_print_string ppf name
    | _ -> Format.fprintf ppf "@[<hov 2>(%s@ %a)@]" name (print_lst emit) args
  end

module Constr = struct
  let int i = Int (Int64.of_int i)
  let node name args = Node { name; args }

  let module_ fields = node "module" fields
end
