module Block_id = struct
  type t = string * int

  let not_id (name, n) = ("!" ^ name, n)

  let name (name, n) = Format.asprintf "%s_%i" name n

  let print ppf id = Format.fprintf ppf "%s" (name id)

  let fresh : string -> t =
    let f = ref 0 in
    fun name ->
      incr f;
      (name, !f)
end

let acceptable_char = function '[' -> '_' | ']' -> '_' | ',' -> '_' | c -> c

module Func_id = struct
  type t =
    | V of string * int
    | Symbol of Symbol.t
    | Caml_curry of int * int
    | Caml_apply of int
    | C_import of string
    | Runtime of string
    | Start

  let name = function
    | V (name, n) -> Format.asprintf "%s_%i" name n
    | Symbol s -> Format.asprintf "%a" Symbol.print s
    | Caml_curry (n, m) ->
      if m = 0 then Format.asprintf "Caml_curry_%i" n
      else Format.asprintf "Caml_curry_%i_%i" n m
    | Caml_apply n -> Format.asprintf "Caml_apply_%i" n
    | C_import s -> Format.asprintf "C_%s" s
    | Runtime s -> Format.asprintf "R_%s" s
    | Start -> "Start"

  let print ppf = function
    | V (name, n) -> Format.fprintf ppf "%s_%i" name n
    | Symbol s -> Symbol.print ppf s
    | Caml_curry (n, m) ->
      if m = 0 then Format.fprintf ppf "Caml_curry_%i" n
      else Format.fprintf ppf "Caml_curry_%i_%i" n m
    | Caml_apply n -> Format.fprintf ppf "Caml_apply_%i" n
    | C_import s -> Format.fprintf ppf "C_%s" s
    | Runtime s -> Format.fprintf ppf "R_%s" s
    | Start -> Format.pp_print_string ppf "Start"

  let of_var_closure_id var =
    let name, id = Variable.unique_name_id var in
    let name = String.map acceptable_char name in
    V (name, id)

  let of_closure_id closure_id =
    let var = Closure_id.unwrap closure_id in
    of_var_closure_id var

  let prim_func_name ({ prim_native_name; prim_name } : Primitive.description) =
    if prim_native_name <> "" then prim_native_name else prim_name

  let prim_name descr = C_import (prim_func_name descr)
end

module Global = struct
  type t =
    | S of string
    | Module_block
    | Closure of Variable.t

  let name = function
    | S name -> Format.asprintf "%s" name
    | Module_block -> Format.asprintf "Module_block"
    | Closure v -> Format.asprintf "Closure_%s" (Variable.unique_name v)

  let print ppf = function
    | S name -> Format.fprintf ppf "G(%s)" name
    | Module_block -> Format.fprintf ppf "Module_block"
    | Closure v -> Format.fprintf ppf "Closure %a" Variable.print v

  let of_symbol s =
    let linkage_name = Symbol.label s in
    let name = Linkage_name.to_string linkage_name in
    let name = String.map acceptable_char name in
    S name
end

module Param = struct
  type t =
    | P of string * int
    | Env

  let print ppf = function
    | P (name, n) -> Format.fprintf ppf "P(%s_%i)" name n
    | Env -> Format.fprintf ppf "Env"

  let name = function
    | P (name, n) -> Format.asprintf "P_%s_%i" name n
    | Env -> "Env"

  let of_var v =
    let n, i = Variable.unique_name_id v in
    P (n, i)
end

module Local = struct
  type var =
    | Variable of Variable.t
    | Set_of_closures of Set_of_closures_id.t
    | Mutable of Mutable_variable.t
    | Fresh of string * int
    | Partial_closure
    | Closure
    | Indirec_call_closure of { arity : int }
    | Block_result of Block_id.t

  let var_name = function
    | Variable v ->
      let name, id = Variable.unique_name_id v in
      let name = String.map acceptable_char name in
      Format.asprintf "%s_%i" name id
    | Set_of_closures id -> Format.asprintf "Set_%a" Set_of_closures_id.print id
    | Mutable v ->
      let name, id = Mutable_variable.unique_name_id v in
      Format.asprintf "%s@%i" name id
    | Fresh (name, n) -> Format.asprintf "%s#%i" name n
    | Partial_closure -> "Partial_closure"
    | Closure -> "Closure"
    | Indirec_call_closure { arity } ->
      Format.asprintf "Indirec_call_closure_%i" arity
    | Block_result id -> Format.asprintf "Block_%a" Block_id.print id

  let print_var ppf var = Format.pp_print_string ppf (var_name var)

  type t =
    | V of var
    | Param of Param.t

  let print ppf = function
    | V v -> print_var ppf v
    | Param p -> Param.print ppf p

  let var_of_var v = Variable v

  let var_of_mut_var v = Mutable v

  let of_var v = V (Variable v)

  let name = function V v -> var_name v | Param p -> Param.name p

  let fresh : string -> var =
    let f = ref 0 in
    fun name ->
      incr f;
      Fresh (name, !f)

  module M = struct
    type nonrec t = var

    let compare = compare
  end

  module Map = Map.Make (M)
end
