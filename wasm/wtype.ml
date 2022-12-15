let print_list f sep ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "%s@ " sep)
    f ppf l

module Var = struct
  module C_import_atom = struct
    type t =
      | I32
      | I64
      | Float
      | Val

    let name_of_atom = function
      | I32 -> "3"
      | I64 -> "6"
      | Float -> "F"
      | Val -> "V"

    let name_of_atoms l = String.concat "" (List.map name_of_atom l)
  end

  type c_import_func_type =
    { params : C_import_atom.t list
    ; results : C_import_atom.t list
    }

  type t =
    | V of string * int
    | Partial_closure of int * int
    | Func of { arity : int }
    | Caml_apply_func of { arity : int }
    | C_import_func of c_import_func_type
    | Closure of
        { arity : int
        ; fields : int
        }
    | Gen_closure of { arity : int }
    | Env
    | Block of { size : int }
    | BlockFloat of { size : int }
      (* This may not work, it is not always possible to distinghuish float
         blocks from float array. This should probaly be replaced by FloatArray *)
    | Set_of_closures of Set_of_closures_id.t
    | Gen_block
    | I31
    | Float
    | Int32
    | Int64
    | Nativeint
    | String
    | Array
    | FloatArray
    | Any
    | Eq

  let name = function
    | V (name, n) -> Format.asprintf "$%s_%i" name n
    | Partial_closure (n, m) -> Format.asprintf "$Partial_closure_%i_%i" n m
    | Env -> Format.asprintf "$Env"
    | Func { arity } -> Format.asprintf "$Func_%i" arity
    | Caml_apply_func { arity } -> Format.asprintf "$Caml_apply_func_%i" arity
    | C_import_func { params; results } ->
      Format.asprintf "$C_import_func_%s_%s"
        (C_import_atom.name_of_atoms params)
        (C_import_atom.name_of_atoms results)
    | Block { size } -> Format.asprintf "$Block_%i" size
    | BlockFloat { size } -> Format.asprintf "$BlockFloat_%i" size
    | Gen_block -> Format.asprintf "$Gen_block"
    | Closure { arity; fields } -> Format.asprintf "$Closure_%i_%i" arity fields
    | Gen_closure { arity } -> Format.asprintf "$Gen_closure_%i" arity
    | Set_of_closures set ->
      Format.asprintf "$Set_of_closures_%a" Set_of_closures_id.print set
    | Float -> "$Float"
    | Int32 -> "$Int32"
    | Int64 -> "$Int64"
    | Nativeint -> "$Nativeint"
    | String -> "$String"
    | Array -> "$Array"
    | FloatArray -> "$FloatArray"
    | Eq -> "eq"
    | Any -> "any"
    | I31 -> "i31"

  let print ppf v = Format.pp_print_string ppf (name v)
end

type atom =
  | I8
  | I16
  | I32
  | I64
  | F64
  | Rvar of Var.t
  | Tuple of atom list

type descr =
  | Struct of
      { sub : Var.t option
      ; fields : atom list
      }
  | Array of
      { sub : Var.t option
      ; fields : atom
      }
  | Func of
      { params : atom list
      ; results : atom list
      }

let rec print_atom ppf = function
  | I8 -> Format.fprintf ppf "i8"
  | I16 -> Format.fprintf ppf "i16"
  | I32 -> Format.fprintf ppf "i32"
  | I64 -> Format.fprintf ppf "i64"
  | F64 -> Format.fprintf ppf "f64"
  | Rvar v -> Format.fprintf ppf "ref_%a" Var.print v
  | Tuple l -> Format.fprintf ppf "Tuple (%a)" (print_list print_atom " ") l

let print_descr ppf = function
  | Struct { sub; fields = atoms } ->
    let pp_sub ppf = function
      | None -> ()
      | Some sub -> Format.fprintf ppf "sub: %a;@ " Var.print sub
    in
    Format.fprintf ppf "@[<hov 2>Struct {%a%a}@]" pp_sub sub
      (print_list print_atom ";")
      atoms
  | Array { sub; fields = atom } ->
    let pp_sub ppf = function
      | None -> ()
      | Some sub -> Format.fprintf ppf "sub: %a;@ " Var.print sub
    in
    Format.fprintf ppf "@[<hov 2>Array {%a%a}@]" pp_sub sub print_atom atom
  | Func { params; results = [] } ->
    Format.fprintf ppf "@[<hov 2>Func {%a}@]" (print_list print_atom ",") params
  | Func { params; results } ->
    Format.fprintf ppf "@[<hov 2>Func {%a} ->@ %a@]"
      (print_list print_atom ",")
      params
      (print_list print_atom ",")
      results
