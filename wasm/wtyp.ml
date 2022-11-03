[@@@ocaml.warning "-23"]

[@@@ocaml.warning "-37"]

[@@@ocaml.warning "-32"]

type mode =
  | Reference
  | Binarien

let mode = Binarien

let print_list f sep ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "%s@ " sep)
    f ppf l

module MSet (M : Set.OrderedType) = struct
  include Set.Make (M)

  let ( += ) r v = r := add v !r
end

module Arity = struct
  type t = int

  module Set = MSet (Int)
end

module Closure_type = struct
  module M = struct
    type t =
      { arity : int
      ; fields : int
      }

    let compare = compare
  end

  include M
  module Set = MSet (M)
end

module C_import = struct
  module M = struct
    type t = Primitive.description

    let compare = compare
  end

  include M
  module Set = MSet (M)
end

module Runtime_import = struct
  module M = struct
    type t =
      { arity : int
      ; name : string
      }

    let compare = compare
  end

  include M
  module Set = MSet (M)
end

module State = struct
  let arities = ref Arity.Set.empty

  let caml_applies = ref Arity.Set.empty

  let block_sizes = ref Arity.Set.empty

  let block_float_sizes = ref Arity.Set.empty

  let closure_types = ref Closure_type.Set.empty

  let c_imports = ref C_import.Set.empty

  let runtime_imports = ref Runtime_import.Set.empty

  let add_arity (i : Arity.t) = Arity.Set.(arities += i)

  let add_caml_apply (i : Arity.t) = Arity.Set.(caml_applies += i)

  let add_block_size i = Arity.Set.(block_sizes += i)

  let add_block_float_size i = Arity.Set.(block_float_sizes += i)

  let add_closure_type ~arity ~fields =
    Closure_type.Set.(closure_types += { arity; fields })

  let add_c_import description = C_import.Set.(c_imports += description)

  let add_runtime_import description =
    Runtime_import.Set.(runtime_imports += description)

  let reset () =
    arities := Arity.Set.empty;
    caml_applies := Arity.Set.empty;
    block_sizes := Arity.Set.singleton 0;
    block_float_sizes := Arity.Set.singleton 0;
    closure_types := Closure_type.Set.empty;
    c_imports := C_import.Set.empty;
    runtime_imports := Runtime_import.Set.empty
end

module Type = struct
  module Var = struct
    type t =
      | V of string * int
      | Partial_closure of int * int
      | Func of { arity : int }
      | Closure of
          { arity : int
          ; fields : int
          }
      | Gen_closure of { arity : int }
      | Env
      | Block of { size : int }
      | BlockFloat of { size : int }
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
      | Block { size } -> Format.asprintf "$Block_%i" size
      | BlockFloat { size } -> Format.asprintf "$BlockFloat_%i" size
      | Gen_block -> Format.asprintf "$Gen_block"
      | Closure { arity; fields } ->
        Format.asprintf "$Closure_%i_%i" arity fields
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
        ; result : atom option
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
    | Func { params; result = None } ->
      Format.fprintf ppf "@[<hov 2>Func {%a}@]"
        (print_list print_atom ",")
        params
    | Func { params; result = Some result } ->
      Format.fprintf ppf "@[<hov 2>Func {%a} ->@ %a@]"
        (print_list print_atom ",")
        params print_atom result
end

let ref_eq = Type.Rvar Eq

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

module Expr = struct
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
        Format.asprintf "%s_%i" name id
      | Set_of_closures id ->
        Format.asprintf "Set_%a" Set_of_closures_id.print id
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

  type nn =
    | S32
    | S64

  type sx =
    | U
    | S

  type irelop =
    | Eq
    | Ne
    | Lt of sx
    | Gt of sx
    | Le of sx
    | Ge of sx

  type binop =
    | I32_add
    | I32_sub
    | I32_mul
    | F64_add
    | F64_sub
    | F64_mul
    | F64_div
    | Ref_eq

  type nv_binop =
    | Struct_set of
        { typ : Type.Var.t
        ; field : int
        }

  type unop =
    | I31_get_s
    | I31_new
    | Struct_get of
        { typ : Type.Var.t
        ; field : int
        }
    | Struct_get_packed of
        { typ : Type.Var.t
        ; field : int
        ; extend : sx
        }
    | Ref_cast_i31
    | Is_i31

  (* Every expression returns exactly one value *)
  type t =
    | Var of Local.t
    | I32 of int32
    | I64 of int64
    | F64 of float
    | Ref_func of Func_id.t
    | Let of
        { var : Local.var
        ; typ : Type.atom
        ; defining_expr : t
        ; body : t
        }
    | If_then_else of
        { cond : t
        ; if_expr : t
        ; else_expr : t
        }
    | I_relop of nn * irelop * (t * t)
    | Binop of binop * (t * t)
    | Unop of unop * t
    | Struct_new of Type.Var.t * t list
    | Array_new_fixed of
        { typ : Type.Var.t
        ; fields : t list
        }
    | Call_ref of
        { typ : Type.Var.t
        ; args : t list
        ; func : t
        }
    | Call of
        { args : t list
        ; func : Func_id.t
        }
    | Ref_cast of
        { typ : Type.Var.t
        ; r : t
        }
    | Global_get of Global.t
    | Seq of no_value_expression list * t
    | Let_cont of
        { cont : Block_id.t
        ; params : (Local.var option * Type.atom) list
        ; handler : t
        ; body : t
        }
    | Apply_cont of
        { cont : Block_id.t
        ; args : t list
        }
    | Br_on_cast of
        { value : t
        ; typ : Type.Var.t
        ; if_cast : Block_id.t
        ; if_else : t
        }
    | Br_if of
        { cond : t
        ; if_true : Block_id.t
        ; if_else : t
        }
    | Br_table of
        { cond : t
        ; cases : Block_id.t list
        ; default : Block_id.t
        }
    | NR of no_return
    | Unit of no_value_expression

  and no_value_expression =
    | NV_seq of no_value_expression list
    | NV_drop of t
    | NV_binop of nv_binop * (t * t)
    | Assign of
        { being_assigned : Local.var
        ; new_value : t
        }
    | Loop of
        { cont : Block_id.t
        ; body : no_value_expression
        }
    | NV_br_if of
        { cond : t
        ; if_true : Block_id.t
        }
    | NV_if_then_else of
        { cond : t
        ; if_expr : no_value_expression
        ; else_expr : no_value_expression
        }
    | NV

  and no_return =
    | NR_let_cont of
        { cont : Block_id.t
        ; params : (Local.var option * Type.atom) list
        ; handler : no_return
        ; body : no_return
        }
    | NR_if_then_else of
        { cond : t
        ; if_expr : no_return
        ; else_expr : no_return
        }
    | NR_br_table of
        { cond : t
        ; cases : Block_id.t list
        ; default : Block_id.t
        }
    | NR_br of
        { cont : Block_id.t
        ; arg : t
        }

  let sx fmt = function
    | U -> Format.fprintf fmt "u"
    | S -> Format.fprintf fmt "s"

  let print_irelop fmt : irelop -> Unit.t = function
    | Eq -> Format.fprintf fmt "eq"
    | Ne -> Format.fprintf fmt "ne"
    | Lt s -> Format.fprintf fmt "lt_%a" sx s
    | Gt s -> Format.fprintf fmt "gt_%a" sx s
    | Le s -> Format.fprintf fmt "le_%a" sx s
    | Ge s -> Format.fprintf fmt "ge_%a" sx s

  let print_nn fmt = function
    | S32 -> Format.fprintf fmt "32"
    | S64 -> Format.fprintf fmt "64"

  let print_binop ppf = function
    | I32_add -> Format.fprintf ppf "I32_add"
    | I32_sub -> Format.fprintf ppf "I32_sub"
    | I32_mul -> Format.fprintf ppf "I32_mul"
    | F64_add -> Format.fprintf ppf "F64_add"
    | F64_sub -> Format.fprintf ppf "F64_sub"
    | F64_mul -> Format.fprintf ppf "F64_mul"
    | F64_div -> Format.fprintf ppf "F64_div"
    | Ref_eq -> Format.fprintf ppf "Ref_eq"

  let print_nv_binop ppf = function
    | Struct_set { typ; field } ->
      Format.fprintf ppf "@[<hov 2>Struct_set(%a).(%i)@]" Type.Var.print typ
        field

  let print_unop ppf = function
    | I31_get_s -> Format.fprintf ppf "I31_get_s"
    | I31_new -> Format.fprintf ppf "I31_new"
    | Struct_get { typ; field } ->
      Format.fprintf ppf "@[<hov 2>Struct_get(%a).(%i)@]" Type.Var.print typ
        field
    | Struct_get_packed { typ; field; extend } ->
      let str = match extend with S -> "_s" | U -> "_u" in
      Format.fprintf ppf "@[<hov 2>Struct_get%s(%a).(%i)@]" str Type.Var.print
        typ field
    | Ref_cast_i31 -> Format.fprintf ppf "Ref_cast_i31"
    | Is_i31 -> Format.fprintf ppf "Is_i31"

  let rec print ppf = function
    | Var l -> Local.print ppf l
    | I32 i -> Format.fprintf ppf "%li" i
    | I64 i -> Format.fprintf ppf "%Li" i
    | F64 f -> Format.fprintf ppf "%g" f
    | Ref_func f -> Format.fprintf ppf "Ref_func %a" Func_id.print f
    | Let { var; defining_expr; body } ->
      Format.fprintf ppf "@[<hov 2>Let %a =@ %a@]@ in@ %a" Local.print_var var
        print defining_expr print body
    | I_relop (nn, op, (arg1, arg2)) ->
      Format.fprintf ppf "@[<hov 2>I_relop(%a_%a:@ %a,@ %a)@]" print_irelop op
        print_nn nn print arg1 print arg2
    | Binop (binop, (arg1, arg2)) ->
      Format.fprintf ppf "@[<hov 2>Binop(%a:@ %a,@ %a)@]" print_binop binop
        print arg1 print arg2
    | Unop (unop, arg) ->
      Format.fprintf ppf "@[<hov 2>Unop(%a:@ %a)@]" print_unop unop print arg
    | Struct_new (typ, args) ->
      Format.fprintf ppf "@[<hov 2>Struct_new(%a:@ %a)@]" Type.Var.print typ
        (print_list print ",") args
    | Array_new_fixed { typ; fields } ->
      Format.fprintf ppf "@[<hov 2>Array_new_fixed(%a:@ %a)@]" Type.Var.print
        typ (print_list print ",") fields
    | Call_ref { typ; args; func } ->
      Format.fprintf ppf "@[<hov 2>Call_ref(%a:@ %a(%a))@]" Type.Var.print typ
        print func (print_list print ",") args
    | Call { args; func } ->
      Format.fprintf ppf "@[<hov 2>Call(%a(%a))@]" Func_id.print func
        (print_list print ",") args
    | Ref_cast { typ; r } ->
      Format.fprintf ppf "@[<hov 2>Ref_cast(%a:@ %a)@]" Type.Var.print typ print
        r
    | Global_get g ->
      Format.fprintf ppf "@[<hov 2>Global_get(%a)@]" Global.print g
    | Seq (effects, last) ->
      Format.fprintf ppf "@[<v 2>Seq(%a;%a)@]"
        (print_list print_no_value ";")
        effects print last
    | If_then_else { cond; if_expr; else_expr } ->
      Format.fprintf ppf "@[<hov 2>If(%a)Then(%a)Else(%a)@]" print cond print
        if_expr print else_expr
    | Let_cont { cont; params; handler; body } ->
      Format.fprintf ppf "@[<hov 2>Let_cont %a(%a) =@ %a@]@ in@ %a"
        Block_id.print cont
        (print_list
           (fun ppf (local, typ) ->
             Format.fprintf ppf "%a : %a"
               (Format.pp_print_option Local.print_var)
               local Type.print_atom typ )
           ", " )
        params print handler print body
    | Apply_cont { cont; args } ->
      Format.fprintf ppf "@[<hov 2>Apply_cont(%a(%a))@]" Block_id.print cont
        (print_list print ",") args
    | Br_on_cast { value; typ; if_cast; if_else } ->
      Format.fprintf ppf "@[<hov 2>Br_on_cast(%a %a -> (%a) else %a)@]" print
        value Type.Var.print typ Block_id.print if_cast print if_else
    | Br_if { cond; if_true; if_else } ->
      Format.fprintf ppf "@[<hov 2>Br_if(%a -> (%a) else %a)@]" print cond
        Block_id.print if_true print if_else
    | Br_table { cond; cases; default } ->
      Format.fprintf ppf "@[<hov 2>Br_table(%a -> (%a) %a@]" print cond
        (print_list Block_id.print " ")
        cases Block_id.print default
    | Unit nv -> Format.fprintf ppf "@[<hov 2>Unit (@ %a@ )@]" print_no_value nv
    | NR nr -> print_no_return ppf nr

  and print_no_value ppf no_value =
    match no_value with
    | NV_seq effects ->
      Format.fprintf ppf "@[<v 2>Seq(%a)@]"
        (print_list print_no_value ";")
        effects
    | NV_drop e -> Format.fprintf ppf "@[<hov 2>Drop (@ %a@ )@]" print e
    | NV_binop (binop, (arg1, arg2)) ->
      Format.fprintf ppf "@[<hov 2>Binop(%a:@ %a,@ %a)@]" print_nv_binop binop
        print arg1 print arg2
    | Assign { being_assigned; new_value } ->
      Format.fprintf ppf "@[<v 2>Assign(%a <- %a)@]" Local.print_var
        being_assigned print new_value
    | Loop { cont; body } ->
      Format.fprintf ppf "@[<hov 2>Loop %a@ %a@]" Block_id.print cont
        print_no_value body
    | NV -> Format.fprintf ppf "Nil"
    | NV_if_then_else { cond; if_expr; else_expr } ->
      Format.fprintf ppf "@[<hov 2>If(%a)Then(%a)Else(%a)@]" print cond
        print_no_value if_expr print_no_value else_expr
    | NV_br_if { cond; if_true } ->
      Format.fprintf ppf "@[<hov 2>Br_if(%a -> (%a))@]" print cond
        Block_id.print if_true

  and print_no_return ppf no_return =
    match no_return with
    | NR_if_then_else { cond; if_expr; else_expr } ->
      Format.fprintf ppf "@[<hov 2>If(%a)Then(%a)Else(%a)@]" print cond
        print_no_return if_expr print_no_return else_expr
    | NR_br_table { cond; cases; default } ->
      Format.fprintf ppf "@[<hov 2>Br_table(%a -> (%a) %a@]" print cond
        (print_list Block_id.print " ")
        cases Block_id.print default
    | NR_let_cont { cont; params; handler; body } ->
      Format.fprintf ppf "@[<hov 2>Let_cont %a(%a) =@ %a@]@ in@ %a"
        Block_id.print cont
        (print_list
           (fun ppf (local, typ) ->
             Format.fprintf ppf "%a : %a"
               (Format.pp_print_option Local.print_var)
               local Type.print_atom typ )
           ", " )
        params print_no_return handler print_no_return body
    | NR_br { cont; arg } ->
      Format.fprintf ppf "@[<hov 2>Br(%a, %a)@]" Block_id.print cont print arg

  let let_ var typ defining_expr body = Let { var; typ; defining_expr; body }

  type function_body =
    | Value of t * Type.atom
    | No_value of no_value_expression

  let required_locals body =
    let add var typ acc =
      match Local.Map.find var acc with
      | prev_typ ->
        assert (typ = prev_typ);
        acc
      | exception Not_found -> Local.Map.add var typ acc
    in
    let let_cont_reqs acc ~cont ~params =
      let acc =
        List.fold_left
          (fun acc (var, typ) ->
            match var with None -> acc | Some var -> add var typ acc )
          acc params
      in
      let acc =
        match (mode, params) with
        | Binarien, _ :: _ :: _ ->
          let var = Local.Block_result cont in
          add var (Type.Tuple (List.map snd params)) acc
        | _ -> acc
      in
      acc
    in
    let rec loop acc = function
      | Var _ | I32 _ | I64 _ | F64 _ | Ref_func _ -> acc
      | Let { var; typ; defining_expr; body } ->
        let acc = add var typ acc in
        let acc = loop acc defining_expr in
        loop acc body
      | I_relop (_, _, (arg1, arg2)) | Binop (_, (arg1, arg2)) ->
        let acc = loop acc arg1 in
        loop acc arg2
      | If_then_else { cond; if_expr; else_expr } ->
        let acc = loop acc cond in
        let acc = loop acc if_expr in
        loop acc else_expr
      | Unop (_op, arg) -> loop acc arg
      | Struct_new (_typ, args) ->
        List.fold_left (fun acc arg -> loop acc arg) acc args
      | Array_new_fixed { typ = _; fields } ->
        List.fold_left (fun acc arg -> loop acc arg) acc fields
      | Call_ref { typ = _; args; func } ->
        List.fold_left (fun acc arg -> loop acc arg) (loop acc func) args
      | Call { args; func = _ } ->
        List.fold_left (fun acc arg -> loop acc arg) acc args
      | Ref_cast { typ = _; r } -> loop acc r
      | Global_get _ -> acc
      | Seq (effects, last) ->
        let acc =
          List.fold_left (fun acc arg -> loop_no_value acc arg) acc effects
        in
        loop acc last
      | Let_cont { cont; params; handler; body } ->
        let acc = let_cont_reqs acc ~cont ~params in
        let acc = loop acc handler in
        loop acc body
      | Apply_cont { cont = _; args } ->
        List.fold_left (fun acc arg -> loop acc arg) acc args
      | Br_on_cast { value; if_cast = _; if_else } ->
        let acc = loop acc value in
        loop acc if_else
      | Br_if { cond; if_true = _; if_else } ->
        let acc = loop acc cond in
        loop acc if_else
      | Br_table { cond; cases = _; default = _ } -> loop acc cond
      | Unit nv -> loop_no_value acc nv
      | NR nr -> loop_no_return acc nr
    and loop_no_value acc nv =
      match nv with
      | NV -> acc
      | NV_seq effects ->
        List.fold_left (fun acc arg -> loop_no_value acc arg) acc effects
      | NV_drop e -> loop acc e
      | NV_binop (_op, (arg1, arg2)) ->
        let acc = loop acc arg1 in
        loop acc arg2
      | Assign { being_assigned = _; new_value } -> loop acc new_value
      | Loop { cont = _; body } -> loop_no_value acc body
      | NV_br_if { cond; if_true = _ } -> loop acc cond
      | NV_if_then_else { cond; if_expr; else_expr } ->
        let acc = loop acc cond in
        let acc = loop_no_value acc if_expr in
        loop_no_value acc else_expr
    and loop_no_return acc nr =
      match nr with
      | NR_if_then_else { cond; if_expr; else_expr } ->
        let acc = loop acc cond in
        let acc = loop_no_return acc if_expr in
        loop_no_return acc else_expr
      | NR_br_table { cond; cases = _; default = _ } -> loop acc cond
      | NR_let_cont { cont; params; handler; body } ->
        let acc = let_cont_reqs acc ~cont ~params in
        let acc = loop_no_return acc handler in
        loop_no_return acc body
      | NR_br { cont = _; arg } -> loop acc arg
    in
    match body with
    | Value (expr, _typ) -> loop Local.Map.empty expr
    | No_value expr -> loop_no_value Local.Map.empty expr
end

module Func = struct
  type t =
    | Decl of
        { params : (Param.t * Type.atom) list
        ; body : Expr.function_body
        }
    | Import of
        { params : Type.atom list
        ; result : Type.atom list
        ; module_ : string
        ; name : string
        }

  let print ppf = function
    | Decl { params; body } ->
      let param ppf (p, typ) =
        Format.fprintf ppf "(%a: %a)" Param.print p Type.print_atom typ
      in
      let print_body ppf = function
        | Expr.Value (e, typ) ->
          Format.fprintf ppf " -> %a@ {@ %a@ }" Type.print_atom typ Expr.print e
        | Expr.No_value e ->
          Format.fprintf ppf "@ {@ %a@ }" Expr.print_no_value e
      in
      Format.fprintf ppf "@[<hov 2>Func (%a)%a@]" (print_list param ",") params
        print_body body
    | Import { params; result; module_; name } ->
      Format.fprintf ppf "@[<hov 2>Import %s %s : (%a) -> %a @]" module_ name
        (print_list Type.print_atom ",")
        params
        (print_list Type.print_atom ",")
        result
end

module Const = struct
  type field =
    | I8 of int
    | I16 of int
    | Ref_func of Func_id.t
    | Global of Global.t
    | I31 of int

  type t =
    | Struct of
        { typ : Type.Var.t
        ; fields : field list
        }
    | Expr of
        { typ : Type.atom
        ; e : Expr.t
        }

  let print_field ppf = function
    | I8 i -> Format.fprintf ppf "i8(%i)" i
    | I16 i -> Format.fprintf ppf "i16(%i)" i
    | I31 i -> Format.fprintf ppf "i31(%i)" i
    | Ref_func f -> Format.fprintf ppf "Ref_func %a" Func_id.print f
    | Global g -> Format.fprintf ppf "%a" Global.print g
end

module Decl = struct
  type t =
    | Type of Type.Var.t * Type.descr
    | Type_rec of (Type.Var.t * Type.descr) list
    | Func of
        { name : Func_id.t
        ; descr : Func.t
        }
    | Const of
        { name : Global.t
        ; descr : Const.t
        }

  let print ppf = function
    | Type (var, descr) ->
      Format.fprintf ppf "type %a = %a" Type.Var.print var Type.print_descr
        descr
    | Type_rec l ->
      let pp ppf (var, descr) =
        Format.fprintf ppf "(%a = %a)" Type.Var.print var Type.print_descr descr
      in
      Format.fprintf ppf "type_rec %a" (print_list pp "") l
    | Func { name; descr } ->
      Format.fprintf ppf "@[<hov 2>func %a =@ %a@]" Func_id.print name
        Func.print descr
    | Const { name; descr = Struct { typ; fields } } ->
      Format.fprintf ppf "@[<hov 2>const %a : %a =@ {%a}@]" Global.print name
        Type.Var.print typ
        (print_list Const.print_field ";")
        fields
    | Const { name; descr = Expr { typ; e } } ->
      Format.fprintf ppf "@[<hov 2>const %a : %a =@ {%a}@]" Global.print name
        Type.print_atom typ Expr.print e
end

module Module = struct
  type t = Decl.t list

  let print ppf l =
    Format.fprintf ppf "@[<v 2>Module {@ %a@ }@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
         Decl.print )
      l
end

module Conv = struct
  type top_env = { offsets : Wasm_closure_offsets.result }

  type env =
    { bound_vars : Variable.Set.t
    ; params : Variable.Set.t
    ; closure_vars : Variable.Set.t
    ; closure_functions : Variable.Set.t
    ; mutables : Mutable_variable.Set.t
    ; current_function : Closure_id.t option
    ; top_env : top_env
    ; catch : Block_id.t Static_exception.Map.t
    }

  let empty_env ~top_env =
    { bound_vars = Variable.Set.empty
    ; params = Variable.Set.empty
    ; closure_vars = Variable.Set.empty
    ; closure_functions = Variable.Set.empty
    ; mutables = Mutable_variable.Set.empty
    ; current_function = None
    ; top_env
    ; catch = Static_exception.Map.empty
    }

  let enter_function ~top_env ~closure_id ~params ~free_vars ~closure_functions
      =
    let params =
      List.fold_left
        (fun params p -> Variable.Set.add (Parameter.var p) params)
        Variable.Set.empty params
    in
    let closure_vars =
      Variable.Set.diff (Variable.Set.diff free_vars params) closure_functions
    in
    { bound_vars = Variable.Set.empty
    ; params
    ; closure_vars
    ; closure_functions
    ; mutables = Mutable_variable.Set.empty
    ; current_function = Some closure_id
    ; top_env
    ; catch = Static_exception.Map.empty
    }

  let bind_catch env catch_id : env * Block_id.t =
    let block_id = Block_id.fresh "catch" in
    ( { env with catch = Static_exception.Map.add catch_id block_id env.catch }
    , block_id )

  module Closure = struct
    let cast r : Expr.t = Ref_cast { typ = Env; r }

    let get_arity e : Expr.t = Unop (Struct_get { typ = Env; field = 0 }, e)

    let get_gen_func e : Expr.t = Unop (Struct_get { typ = Env; field = 1 }, e)

    let get_direct_func e ~arity : Expr.t =
      if arity = 1 then get_gen_func e
      else Unop (Struct_get { typ = Gen_closure { arity }; field = 2 }, e)

    let project_closure ?(cast : unit option) (top_env : top_env) closure_id
        set_of_closures : Expr.t =
      let accessor =
        Closure_id.Map.find closure_id top_env.offsets.function_accessors
      in
      if not accessor.recursive_set then set_of_closures
      else
        let typ : Type.Var.t = Set_of_closures accessor.set in
        let set_of_closures =
          match cast with
          | None -> set_of_closures
          | Some () -> Expr.Ref_cast { typ; r = set_of_closures }
        in
        Unop (Struct_get { typ; field = accessor.field }, set_of_closures)

    let project_var ?(cast : unit option) (top_env : top_env) closure_id var
        closure : Expr.t =
      let accessor =
        try
          Var_within_closure.Map.find var
            top_env.offsets.free_variable_accessors
        with Not_found ->
          failwith
            (Format.asprintf "Missing var_within_closure %a"
               Var_within_closure.print var )
      in
      let closure_info =
        Closure_id.Map.find closure_id top_env.offsets.function_accessors
      in
      if not accessor.recursive_set then begin
        let typ : Type.Var.t =
          Closure { arity = closure_info.arity; fields = accessor.closure_size }
        in
        let closure : Expr.t =
          match cast with
          | None -> closure
          | Some () -> Ref_cast { typ; r = closure }
        in
        State.add_closure_type ~arity:closure_info.arity
          ~fields:accessor.closure_size;
        Unop (Struct_get { typ; field = accessor.field }, closure)
      end
      else
        let closure_typ : Type.Var.t =
          Closure { arity = closure_info.arity; fields = 1 }
        in
        let closure : Expr.t =
          match cast with
          | None -> closure
          | Some () -> Ref_cast { typ = closure_typ; r = closure }
        in
        State.add_closure_type ~arity:closure_info.arity ~fields:1;
        let set_typ : Type.Var.t = Set_of_closures closure_info.set in
        let set_of_closures : Expr.t =
          let field = if closure_info.arity = 1 then 2 else 3 in
          Ref_cast
            { typ = set_typ
            ; r = Unop (Struct_get { typ = closure_typ; field }, closure)
            }
        in
        Unop
          (Struct_get { typ = set_typ; field = accessor.field }, set_of_closures)

    let move_within_set_of_closures ?(cast : unit option) (top_env : top_env)
        ~start_from ~move_to closure : Expr.t =
      if Closure_id.equal start_from move_to then closure
      else begin
        let start_from_info =
          Closure_id.Map.find start_from top_env.offsets.function_accessors
        in
        let move_to_info =
          Closure_id.Map.find move_to top_env.offsets.function_accessors
        in
        let closure_typ : Type.Var.t =
          Closure { arity = start_from_info.arity; fields = 1 }
        in
        let closure : Expr.t =
          match cast with
          | None -> closure
          | Some () -> Ref_cast { typ = closure_typ; r = closure }
        in
        State.add_closure_type ~arity:start_from_info.arity ~fields:1;
        let set_typ : Type.Var.t = Set_of_closures start_from_info.set in
        let set_of_closures : Expr.t =
          let field = if start_from_info.arity = 1 then 2 else 3 in
          Ref_cast
            { typ = set_typ
            ; r = Unop (Struct_get { typ = closure_typ; field }, closure)
            }
        in
        Unop
          ( Struct_get { typ = set_typ; field = move_to_info.field }
          , set_of_closures )
      end
  end

  module Block = struct
    let get_tag ?(cast : unit option) e : Expr.t =
      let block : Expr.t =
        match cast with
        | None -> e
        | Some () -> Ref_cast { r = e; typ = Gen_block }
      in
      Unop (Struct_get_packed { extend = U; typ = Gen_block; field = 0 }, block)

    let get_field ?(cast : unit option) e ~field : Expr.t =
      let size = field + 1 in
      State.add_block_size size;
      let typ : Type.Var.t = Block { size } in
      let e =
        match cast with None -> e | Some () -> Expr.(Ref_cast { typ; r = e })
      in
      Unop (Struct_get { typ; field = field + 2 }, e)

    let set_field ?(cast : unit option) ~block value ~field :
        Expr.no_value_expression =
      let size = field + 1 in
      State.add_block_size size;
      let typ : Type.Var.t = Block { size } in
      let block =
        match cast with
        | None -> block
        | Some () -> Expr.(Ref_cast { typ; r = block })
      in
      NV_binop (Struct_set { typ; field = field + 2 }, (block, value))
  end

  module WInt = struct
    let untag e : Expr.t = Unop (I31_get_s, Unop (Ref_cast_i31, e))

    let tag e : Expr.t = Unop (I31_new, e)
  end

  let const_float f : Expr.t = Struct_new (Float, [ F64 f ])

  let const_int32 i : Expr.t = Struct_new (Int32, [ I32 i ])

  let const_int64 i : Expr.t = Struct_new (Int64, [ I64 i ])

  let const_nativeint i : Expr.t =
    Struct_new (Nativeint, [ I32 (Nativeint.to_int32 i) ])

  let const_string s : Expr.t =
    let fields =
      String.fold_right
        (fun c l -> Expr.I32 (Int32.of_int (Char.code c)) :: l)
        s []
    in
    Array_new_fixed { typ = String; fields }

  let bind_var env var =
    { env with bound_vars = Variable.Set.add var env.bound_vars }

  let bind_mutable_var env var =
    { env with mutables = Mutable_variable.Set.add var env.mutables }

  let conv_var (env : env) (var : Variable.t) : Expr.t =
    begin
      if Variable.Set.mem var env.params then
        Var (Expr.Local.Param (Param.of_var var))
      else if Variable.Set.mem var env.bound_vars then
        Var (Expr.Local.of_var var)
      else
        match env.current_function with
        | None -> assert false
        | Some closure_id ->
          if Variable.Set.mem var env.closure_vars then
            Closure.project_var ~cast:() env.top_env closure_id
              (Var_within_closure.wrap var)
              (Var (Param Env))
          else if Variable.Set.mem var env.closure_functions then
            let move_to = Closure_id.wrap var in
            Closure.move_within_set_of_closures ~cast:() env.top_env
              ~start_from:closure_id ~move_to (Var (Param Env))
          else Misc.fatal_errorf "Unbound variable %a" Variable.print var
    end

  let dummy_const = 123456789

  let dummy_value : Expr.t = Unop (I31_new, I32 (Int32.of_int dummy_const))

  let unit_value : Expr.t = Unop (I31_new, I32 0l)

  let true_value : Expr.t = Unop (I31_new, I32 1l)

  let false_value : Expr.t = Unop (I31_new, I32 0l)

  let bool_not e : Expr.t = Binop (I32_sub, (I32 1l, e))

  let rec expr_is_pure (e : Expr.t) =
    match e with
    | Var _ | I32 _ | I64 _ | F64 _ | Global_get _ -> true
    | Unop (I31_new, e) -> expr_is_pure e
    | Let { defining_expr; body } ->
      expr_is_pure defining_expr && expr_is_pure body
    | _ -> false

  let drop (e : Expr.t) : Expr.no_value_expression =
    match e with Unit nv -> nv | e -> NV_drop e

  let seq l : Expr.t =
    match l with
    | [ e ] -> e
    | _ ->
      let rec split_last l =
        match l with
        | [] -> assert false
        | [ v ] -> ([], v)
        | h :: t ->
          let l, last = split_last t in
          (h :: l, last)
      in
      let l, last = split_last l in
      Seq (List.map drop l, last)

  let const_block ~symbols_being_bound tag fields :
      Const.t * (int * Symbol.t) list =
    let size = List.length fields in
    State.add_block_size size;
    let fields_to_update = ref [] in
    let field i (f : Flambda.constant_defining_value_block_field) : Const.field
        =
      match f with
      | Symbol s ->
        if Symbol.Set.mem s symbols_being_bound then begin
          fields_to_update := (i, s) :: !fields_to_update;
          I31 dummy_const
        end
        else Global (Global.of_symbol s)
      | Const (Int i) -> I31 i
      | Const (Char c) -> I31 (Char.code c)
    in
    let fields =
      [ Const.I8 (Tag.to_int tag); Const.I16 size ] @ List.mapi field fields
    in
    (Struct { typ = Type.Var.Block { size }; fields }, !fields_to_update)

  let box_float x : Expr.t = Struct_new (Type.Var.Float, [ x ])

  let unbox_float x : Expr.t =
    let typ = Type.Var.Float in
    Unop (Struct_get { typ; field = 0 }, Ref_cast { typ; r = x })

  let box_int (kind : Primitive.boxed_integer) x : Expr.t =
    let typ : Type.Var.t =
      match kind with
      | Pint32 -> Int32
      | Pint64 -> Int64
      | Pnativeint -> Nativeint
    in
    Struct_new (typ, [ x ])

  let unbox_int (kind : Primitive.boxed_integer) x : Expr.t =
    let typ : Type.Var.t =
      match kind with
      | Pint32 -> Int32
      | Pint64 -> Int64
      | Pnativeint -> Nativeint
    in
    Unop (Struct_get { typ; field = 0 }, Ref_cast { typ; r = x })

  let conv_apply env (apply : Flambda.apply) : Expr.t =
    match apply.kind with
    | Indirect -> begin
      match apply.args with
      | [] -> assert false
      | [ arg ] ->
        let func_typ = Type.Var.Func { arity = 1 } in
        let var : Expr.Local.var = Indirec_call_closure { arity = 1 } in
        let closure : Expr.t = Closure.cast (conv_var env apply.func) in
        let func : Expr.t = Closure.get_gen_func (Var (V var)) in
        let args : Expr.t list = [ conv_var env arg; Var (V var) ] in
        Let
          { var
          ; typ = Rvar Env
          ; defining_expr = closure
          ; body = Call_ref { typ = func_typ; func; args }
          }
      | _ :: _ :: _ ->
        let arity = List.length apply.args in
        let args =
          Closure.cast (conv_var env apply.func)
          :: List.map (conv_var env) apply.args
        in
        State.add_caml_apply arity;
        Call { func = Caml_apply arity; args }
    end
    | Direct closure_id ->
      let func = Func_id.of_closure_id closure_id in
      let args =
        List.map (conv_var env) apply.args
        @ [ Closure.cast (conv_var env apply.func) ]
      in
      Call { func; args }

  let conv_allocated_const_expr (const : Allocated_const.t) : Type.atom * Expr.t
      =
    match const with
    | Float f -> (Rvar Float, const_float f)
    | Int32 i -> (Rvar Int32, const_int32 i)
    | Int64 i -> (Rvar Int64, const_int64 i)
    | Nativeint i -> (Rvar Nativeint, const_nativeint i)
    | Immutable_string s | String s -> (Rvar String, const_string s)
    | Float_array _ | Immutable_float_array _ ->
      failwith
        (Format.asprintf "TODO allocated const %a" Allocated_const.print const)

  let conv_allocated_const (const : Allocated_const.t) : Const.t =
    let typ, e = conv_allocated_const_expr const in
    Expr { typ; e }

  let closure_type (set_of_closures : Flambda.set_of_closures) =
    let Flambda.{ function_decls; free_vars } = set_of_closures in
    let is_recursive = Variable.Map.cardinal function_decls.funs > 1 in
    if not is_recursive then None
    else begin
      let func_types =
        Variable.Map.fold
          (fun _id (function_decl : Flambda.function_declaration) acc ->
            let arity = Flambda_utils.function_arity function_decl in
            let typ : Type.atom = Rvar (Closure { arity; fields = 1 }) in
            typ :: acc )
          function_decls.funs []
      in
      let rev_fields =
        Variable.Map.fold
          (fun _id _var acc ->
            let typ : Type.atom = ref_eq in
            typ :: acc )
          free_vars func_types
      in
      let descr : Type.descr =
        Struct { sub = None; fields = List.rev rev_fields }
      in
      let name : Type.Var.t =
        Set_of_closures function_decls.set_of_closures_id
      in
      Some (Decl.Type (name, descr))
    end

  let conv_is_int expr : Expr.t =
    match mode with
    | Binarien -> Unop (I31_new, Unop (Is_i31, expr))
    | Reference ->
      let cont = Block_id.fresh "isint" in
      Let_cont
        { cont
        ; params = [ (None, Type.Rvar I31) ]
        ; handler = true_value
        ; body =
            Br_on_cast
              { value = expr; typ = I31; if_cast = cont; if_else = false_value }
        }

  let closure_types (program : Flambda.program) =
    List.filter_map closure_type (Flambda_utils.all_sets_of_closures program)

  let rec conv_body (env : top_env) (expr : Flambda.program_body) effects :
      Module.t =
    match expr with
    | Let_symbol (symbol, Set_of_closures set, body) ->
      let decl = closed_function_declarations symbol set.function_decls in
      let body = conv_body env body effects in
      decl @ body
    | Let_symbol (symbol, const, body) ->
      let decls, new_effects =
        conv_symbol ~symbols_being_bound:Symbol.Set.empty symbol const
      in
      assert (new_effects = []);
      let body = conv_body env body effects in
      decls @ body
    | Let_rec_symbol (decls, body) ->
      let symbols_being_bound =
        List.fold_left
          (fun set (symbol, _) -> Symbol.Set.add symbol set)
          Symbol.Set.empty decls
      in
      let decls, effects =
        List.fold_left
          (fun (decls, effects) (symbol, const) ->
            let decl, new_effecs =
              conv_symbol ~symbols_being_bound symbol const
            in
            (decl @ decls, new_effecs @ effects) )
          ([], effects) decls
      in
      let body = conv_body env body effects in
      decls @ body
    | Initialize_symbol (symbol, tag, fields, body) ->
      let decl, effect = conv_initialize_symbol env symbol tag fields in
      decl :: conv_body env body (effect @ effects)
    | Effect (expr, body) ->
      let expr_env = empty_env ~top_env:env in
      let effect : Expr.t = conv_expr expr_env expr in
      conv_body env body (drop effect :: effects)
    | End _end_symbol ->
      [ Decl.Func
          { name = Start
          ; descr =
              Decl { params = []; body = No_value (NV_seq (List.rev effects)) }
          }
      ]

  and conv_initialize_symbol env symbol tag fields :
      _ * Expr.no_value_expression list =
    let size = List.length fields in
    let fields =
      List.mapi
        (fun i field ->
          (i, field, Initialize_symbol_to_let_symbol.constant_field field) )
        fields
    in
    let fields_to_update = ref [] in
    let predefined_fields =
      List.map
        (fun (i, expr, field) : Const.field ->
          match field with
          | None ->
            let expr_env = empty_env ~top_env:env in
            let expr = conv_expr expr_env expr in
            fields_to_update := (i, expr) :: !fields_to_update;
            I31 dummy_const
          | Some (field : Flambda.constant_defining_value_block_field) -> (
            match field with
            | Symbol s -> Global (Global.of_symbol s)
            | Const (Int i) -> I31 i
            | Const (Char c) -> I31 (Char.code c) ) )
        fields
    in
    let name = Global.of_symbol symbol in
    let descr : Const.t =
      let fields =
        [ Const.I8 (Tag.to_int tag); Const.I16 size ] @ predefined_fields
      in
      Struct { typ = Type.Var.Block { size }; fields }
    in
    let decl = Decl.Const { name; descr } in
    let size = List.length fields in
    State.add_block_size size;
    let effect (field, expr) : Expr.no_value_expression =
      Block.set_field ~field
        ~block:(Expr.Global_get (Global.of_symbol symbol))
        expr
    in
    let effect = List.map effect !fields_to_update in
    (decl, effect)

  and conv_symbol ~symbols_being_bound symbol
      (const : Flambda.constant_defining_value) :
      Decl.t list * Expr.no_value_expression list =
    match const with
    | Block (tag, fields) ->
      let name = Global.of_symbol symbol in
      let descr, fields_to_update =
        const_block ~symbols_being_bound tag fields
      in
      let new_effects =
        List.map
          (fun (field_to_update, field_contents) : Expr.no_value_expression ->
            Block.set_field ~field:field_to_update
              ~block:(Expr.Global_get (Global.of_symbol symbol))
              (Expr.Global_get (Global.of_symbol field_contents)) )
          fields_to_update
      in
      ([ Const { name; descr } ], new_effects)
    | Project_closure (_sym, _closure_id) -> ([], [])
    | Set_of_closures set ->
      let decl = closed_function_declarations symbol set.function_decls in
      (decl, [])
    | Allocated_const const ->
      let name = Global.of_symbol symbol in
      let descr = conv_allocated_const const in
      ([ Const { name; descr } ], [])

  and closed_function_declarations _symbol
      (declarations : Flambda.function_declarations) : Decl.t list =
    Variable.Map.fold
      (fun name (declaration : Flambda.function_declaration) declarations ->
        let function_name = Func_id.of_var_closure_id name in
        let arity = List.length declaration.params in
        let closure =
          let fields : Const.field list =
            State.add_arity arity;
            State.add_closure_type ~arity ~fields:0;
            if arity = 1 then [ I8 1; Ref_func function_name ]
            else
              [ I8 arity
              ; Ref_func (Func_id.Caml_curry (arity, 0))
              ; Ref_func function_name
              ]
          in
          Const.Struct { typ = Type.Var.Closure { arity; fields = 0 }; fields }
        in
        let closure_name =
          let closure_symbol =
            Compilenv.closure_symbol (Closure_id.wrap name)
          in
          Global.of_symbol closure_symbol
        in
        Decl.Const { name = closure_name; descr = closure } :: declarations )
      declarations.funs []

  and conv_set_of_closures env (set_of_closures : Flambda.set_of_closures) :
      Expr.t =
    let function_decls = set_of_closures.function_decls in
    let is_recursive = Variable.Map.cardinal function_decls.funs > 1 in
    if not is_recursive then begin
      let func_var, function_decl = Variable.Map.choose function_decls.funs in
      let arity = Flambda_utils.function_arity function_decl in
      let fields = Variable.Map.cardinal set_of_closures.free_vars in
      State.add_closure_type ~arity ~fields;
      let typ : Type.Var.t = Closure { arity; fields } in
      let rev_value_fields =
        Variable.Map.fold
          (fun _id (var : Flambda.specialised_to) acc ->
            conv_var env var.var :: acc )
          set_of_closures.free_vars []
      in
      let func_id = Func_id.of_var_closure_id func_var in
      let head_fields =
        if arity = 1 then Expr.[ I32 1l; Ref_func func_id ]
        else
          Expr.
            [ I32 (Int32.of_int arity)
            ; Ref_func (Func_id.Caml_curry (arity, 0))
            ; Ref_func func_id
            ]
      in
      Expr.Struct_new (typ, head_fields @ List.rev rev_value_fields)
    end
    else begin
      let add_closure func_var (function_decl : Flambda.function_declaration)
          body : Expr.t =
        let arity = Flambda_utils.function_arity function_decl in
        let typ : Type.Var.t = Closure { arity; fields = 1 } in
        let defining_expr : Expr.t =
          let func_id = Func_id.of_var_closure_id func_var in
          let fields =
            if arity = 1 then Expr.[ I32 1l; Ref_func func_id; dummy_value ]
            else
              Expr.
                [ I32 (Int32.of_int arity)
                ; Ref_func (Func_id.Caml_curry (arity, 0))
                ; Ref_func func_id
                ; dummy_value
                ]
          in
          Expr.Struct_new (typ, fields)
        in
        Let { var = Variable func_var; typ = Rvar typ; defining_expr; body }
      in
      let set_var : Expr.Local.var =
        Set_of_closures function_decls.set_of_closures_id
      in
      let update_fields func_var (function_decl : Flambda.function_declaration)
          updates : Expr.no_value_expression list =
        let arity = Flambda_utils.function_arity function_decl in
        let typ : Type.Var.t = Closure { arity; fields = 1 } in
        let field = if arity = 1 then 2 else 3 in
        NV_binop
          ( Struct_set { typ; field }
          , (Var (V (Variable func_var)), Var (V set_var)) )
        :: updates
      in
      let update_fields =
        Variable.Map.fold update_fields function_decls.funs []
      in
      let build_set body : Expr.t =
        let typ : Type.Var.t =
          Set_of_closures function_decls.set_of_closures_id
        in
        let func_fields =
          List.map
            (fun (func_var, _) : Expr.t -> Var (V (Variable func_var)))
            (Variable.Map.bindings function_decls.funs)
        in
        let free_vars =
          List.map
            (fun (_, (var : Flambda.specialised_to)) -> conv_var env var.var)
            (Variable.Map.bindings set_of_closures.free_vars)
        in
        let defining_expr = Expr.Struct_new (typ, func_fields @ free_vars) in
        Let { var = set_var; typ = Rvar typ; defining_expr; body }
      in
      let expr = Expr.Seq (update_fields, Expr.Var (V set_var)) in
      let expr = build_set expr in
      Variable.Map.fold add_closure function_decls.funs expr
    end

  and conv_function_declaration ~top_env ~closure_functions function_name
      (declaration : Flambda.function_declaration) : Decl.t =
    let arity = List.length declaration.params in
    let closure_id = Closure_id.wrap function_name in
    State.add_arity arity;
    let params =
      List.map
        (fun p -> (Param.of_var (Parameter.var p), ref_eq))
        declaration.params
    in
    let env =
      enter_function ~closure_id ~params:declaration.params
        ~free_vars:declaration.free_variables ~closure_functions ~top_env
    in
    let body = conv_expr env declaration.body in
    let func =
      Func.Decl
        { params = params @ [ (Param.Env, Type.Rvar Type.Var.Env) ]
        ; body = Value (body, ref_eq)
        }
    in
    let name = Func_id.of_var_closure_id function_name in
    Decl.Func { name; descr = func }

  and conv_expr (env : env) (expr : Flambda.t) : Expr.t =
    match expr with
    | Let { var; defining_expr; body = Var v; _ } when Variable.equal var v ->
      conv_named env defining_expr
    | Let { var; defining_expr; body; _ } ->
      let local = Expr.Local.var_of_var var in
      let defining_expr = conv_named env defining_expr in
      let body = conv_expr (bind_var env var) body in
      Let { var = local; typ = ref_eq; defining_expr; body }
    | Var var -> conv_var env var
    | Apply apply -> conv_apply env apply
    | Let_mutable { var; initial_value; contents_kind = _; body } ->
      let local = Expr.Local.var_of_mut_var var in
      let defining_expr = conv_var env initial_value in
      let body = conv_expr (bind_mutable_var env var) body in
      Let { var = local; typ = ref_eq; defining_expr; body }
    | Assign { being_assigned; new_value } ->
      assert (Mutable_variable.Set.mem being_assigned env.mutables);
      let being_assigned = Expr.Local.var_of_mut_var being_assigned in
      let new_value = conv_var env new_value in
      Unit (Assign { being_assigned; new_value })
    | If_then_else (var, if_expr, else_expr) ->
      let cond : Expr.t = WInt.untag (conv_var env var) in

      let if_expr = conv_expr env if_expr in
      let else_expr = conv_expr env else_expr in
      If_then_else { cond; if_expr; else_expr }
    | Static_catch (id, params, body, handler) ->
      let body_env, cont = bind_catch env id in
      let handler_env = List.fold_left bind_var env params in
      let params =
        List.map (fun v -> (Some (Expr.Local.var_of_var v), ref_eq)) params
      in
      let handler = conv_expr handler_env handler in
      let body = conv_expr body_env body in
      Let_cont { cont; params; handler; body }
    | Static_raise (id, args) ->
      let cont =
        try Static_exception.Map.find id env.catch
        with Not_found ->
          Misc.fatal_errorf "Unbound static exception %a" Static_exception.print
            id
      in
      let args = List.map (conv_var env) args in
      Apply_cont { cont; args }
    | While (cond, body) ->
      let cond : Expr.t = WInt.untag (conv_expr env cond) in
      let cont = Block_id.fresh "continue" in
      let body : Expr.no_value_expression =
        NV_seq [ drop (conv_expr env body); NV_br_if { cond; if_true = cont } ]
      in
      Unit
        (NV_if_then_else { cond; if_expr = Loop { cont; body }; else_expr = NV })
    | For { bound_var; from_value; to_value; direction; body } ->
      let cont = Block_id.fresh "continue" in
      let loop_local = Expr.Local.fresh "for_counter" in
      let local = Expr.Local.var_of_var bound_var in
      let to_value_local = Expr.Local.fresh "for_end" in

      let next : Expr.t =
        let op : Expr.binop =
          match direction with Upto -> I32_add | Downto -> I32_sub
        in
        Binop (op, (Var (V loop_local), I32 1l))
      in
      let cond : Expr.t =
        let dir : Expr.irelop =
          match direction with Upto -> Le S | Downto -> Ge S
        in
        I_relop (S32, dir, (Var (V loop_local), Var (V to_value_local)))
      in

      let body : Expr.no_value_expression =
        let env = bind_var env bound_var in
        NV_seq
          [ drop (conv_expr env body)
          ; Assign { being_assigned = loop_local; new_value = next }
          ; Assign
              { being_assigned = local
              ; new_value = WInt.tag (Var (V loop_local))
              }
          ; NV_br_if { cond; if_true = cont }
          ]
      in
      let body : Expr.t =
        Unit
          (NV_if_then_else
             { cond; if_expr = Loop { cont; body }; else_expr = NV } )
      in
      Let
        { var = local
        ; typ = ref_eq
        ; defining_expr = conv_var env from_value
        ; body =
            Let
              { var = to_value_local
              ; typ = I32
              ; defining_expr = WInt.untag (conv_var env to_value)
              ; body =
                  Let
                    { var = loop_local
                    ; typ = I32
                    ; defining_expr = WInt.untag (Var (V local))
                    ; body
                    }
              }
        }
    | Switch (cond, switch) ->
      let cond = conv_var env cond in
      conv_switch env cond switch
    | _ ->
      let msg = Format.asprintf "TODO (conv_expr) %a" Flambda.print expr in
      failwith msg

  and conv_switch (env : env) (cond : Expr.t) (switch : Flambda.switch) : Expr.t
      =
    let default_id = Block_id.fresh "switch_default" in
    let branches _set cases =
      let cases, defs =
        List.fold_left
          (fun (map, defs) (i, branch) ->
            let id = Block_id.fresh (Printf.sprintf "switch_%i" i) in
            (Numbers.Int.Map.add i id map, (id, branch) :: defs) )
          (Numbers.Int.Map.empty, [])
          cases
      in
      let max_branch, default_branch =
        let max, max_branch = Numbers.Int.Map.max_binding cases in
        ( max
        , match switch.failaction with
          | None -> max_branch
          | Some _ -> default_id )
      in
      let cases =
        (* TODO max_branch should be sufficient sometimes: the default
           can replace the last case *)
        List.init (max_branch + 1) (fun i ->
            match Numbers.Int.Map.find_opt i cases with
            | None -> default_branch
            | Some branch -> branch )
      in
      (cases, defs)
    in
    let default_defs =
      match switch.failaction with
      | None -> []
      | Some default -> [ (default_id, default) ]
    in
    let br_table get_int cases defs : Expr.no_return =
      let default_branch : Block_id.t =
        match switch.failaction with
        | None -> fst (List.hd defs)
        | Some _ -> default_id
      in
      let cond : Expr.t = get_int cond in
      NR_br_table { cond; cases; default = default_branch }
    in
    let make_let_conts body defs : Expr.t =
      let fallthrough = Block_id.fresh "switch_result" in
      let add_def (body : Expr.no_return) (cont, branch) : Expr.no_return =
        NR_let_cont
          { cont
          ; params = []
          ; body
          ; handler = NR_br { cont = fallthrough; arg = conv_expr env branch }
          }
      in
      let body = List.fold_left add_def body defs in
      let param = Expr.Local.fresh "switch_result" in
      Let_cont
        { cont = fallthrough
        ; params = [ (Some param, ref_eq) ]
        ; body = NR body
        ; handler = Var (V param)
        }
    in
    if Numbers.Int.Set.is_empty switch.numconsts then
      let block_cases, block_defs = branches switch.numblocks switch.blocks in
      let defs = block_defs @ default_defs in
      make_let_conts
        (br_table (Block.get_tag ~cast:()) block_cases block_defs)
        defs
    else if Numbers.Int.Set.is_empty switch.numblocks then
      let const_cases, const_defs = branches switch.numconsts switch.consts in
      let defs = const_defs @ default_defs in
      make_let_conts (br_table WInt.untag const_cases const_defs) defs
    else
      let const_cases, const_defs = branches switch.numconsts switch.consts in
      let block_cases, block_defs = branches switch.numblocks switch.blocks in
      let defs = const_defs @ block_defs @ default_defs in
      let body : Expr.no_return =
        let if_expr = br_table WInt.untag const_cases const_defs in
        let else_expr =
          br_table (Block.get_tag ~cast:()) block_cases block_defs
        in
        NR_if_then_else
          { cond =
              (* TODO refactor things to avoid this tagging/untagging and useless branches in is_int*)
              WInt.untag (conv_is_int cond)
          ; if_expr
          ; else_expr
          }
      in
      make_let_conts body defs

  and conv_named (env : env) (named : Flambda.named) : Expr.t =
    match named with
    | Prim (prim, args, _dbg) -> conv_prim env ~prim ~args
    | Symbol s -> Global_get (Global.of_symbol s)
    | Expr (Var var) -> conv_var env var
    | Const c ->
      let c = match c with Int i -> i | Char c -> Char.code c in
      Unop (I31_new, I32 (Int32.of_int c))
    | Expr e -> conv_expr env e
    | Read_symbol_field (symbol, field) ->
      Block.get_field ~field Expr.(Global_get (Global.of_symbol symbol))
    | Read_mutable mut_var -> Var (V (Expr.Local.var_of_mut_var mut_var))
    | Project_var project_var ->
      let closure = conv_var env project_var.closure in
      Closure.project_var env.top_env project_var.closure_id project_var.var
        closure
    | Project_closure project_closure ->
      let set_of_closures = conv_var env project_closure.set_of_closures in
      Closure.project_closure ~cast:() env.top_env project_closure.closure_id
        set_of_closures
    | Move_within_set_of_closures move ->
      let closure = conv_var env move.closure in
      Closure.move_within_set_of_closures ~cast:() env.top_env
        ~start_from:move.start_from ~move_to:move.move_to closure
    | Set_of_closures set -> conv_set_of_closures env set
    | Allocated_const const ->
      let _typ, e = conv_allocated_const_expr const in
      e

  and conv_prim env ~(prim : Clambda_primitives.primitive) ~args : Expr.t =
    let args = List.map (conv_var env) args in
    let arg1 args =
      match args with
      | [ a ] -> a
      | _ -> Misc.fatal_errorf "Wrong number of primitive arguments"
    in
    let args2 args =
      match args with
      | [ a; b ] -> (a, b)
      | _ -> Misc.fatal_errorf "Wrong number of primitive arguments"
    in
    let i32 v = WInt.untag v in
    let i31 v = WInt.tag v in
    let runtime_prim name : Expr.t =
      let arity = List.length args in
      State.add_runtime_import { name; arity };
      let func : Func_id.t = Runtime name in
      Call { func; args }
    in
    match prim with
    | Paddint -> i31 (Expr.Binop (I32_add, args2 (List.map i32 args)))
    | Psubint -> i31 (Expr.Binop (I32_sub, args2 (List.map i32 args)))
    | Pmulint -> i31 (Expr.Binop (I32_mul, args2 (List.map i32 args)))
    | Paddfloat ->
      box_float (Expr.Binop (F64_add, args2 (List.map unbox_float args)))
    | Psubfloat ->
      box_float (Expr.Binop (F64_sub, args2 (List.map unbox_float args)))
    | Pmulfloat ->
      box_float (Expr.Binop (F64_mul, args2 (List.map unbox_float args)))
    | Pdivfloat ->
      box_float (Expr.Binop (F64_div, args2 (List.map unbox_float args)))
    | Pccall descr ->
      let unbox_arg (t : Primitive.native_repr) arg =
        match t with
        | Same_as_ocaml_repr -> arg
        | Unboxed_float -> unbox_float arg
        | Unboxed_integer kind -> unbox_int kind arg
        | Untagged_int -> i32 arg
      in
      let box_result (t : Primitive.native_repr) res =
        match t with
        | Same_as_ocaml_repr -> res
        | Unboxed_float -> box_float res
        | Unboxed_integer kind -> box_int kind res
        | Untagged_int -> i31 res
      in
      State.add_c_import descr;
      let args = List.map2 unbox_arg descr.prim_native_repr_args args in
      box_result descr.prim_native_repr_res
        (Call { args; func = Func_id.prim_name descr })
    | Pmakeblock (tag, _mut, _shape) ->
      let size = List.length args in
      State.add_block_size size;
      Struct_new
        ( Block { size }
        , I32 (Int32.of_int tag) :: I32 (Int32.of_int size) :: args )
    | Pfield field ->
      let arg = arg1 args in
      Block.get_field ~field ~cast:() arg
    | Psetfield (field, _kind, _init) ->
      let block, value = args2 args in
      Seq ([ Block.set_field ~cast:() ~field ~block value ], unit_value)
    | Popaque -> arg1 args
    | Pisint -> conv_is_int (arg1 args)
    | Pintcomp Ceq -> i31 (Expr.Binop (Ref_eq, args2 args))
    | Pintcomp cop -> begin
      let op : Expr.t =
        match cop with
        | Ceq -> Binop (Ref_eq, args2 args)
        | Cne -> bool_not (Binop (Ref_eq, args2 args))
        | Clt -> I_relop (S32, Lt S, args2 (List.map i32 args))
        | Cgt -> I_relop (S32, Gt S, args2 (List.map i32 args))
        | Cle -> I_relop (S32, Le S, args2 (List.map i32 args))
        | Cge -> I_relop (S32, Ge S, args2 (List.map i32 args))
      in
      i31 op
    end
    | Pcompare_ints -> runtime_prim "compare_ints"
    | Pcompare_floats -> runtime_prim "compare_floats"
    | _ ->
      let msg =
        Format.asprintf "TODO prim %a" Printclambda_primitives.primitive prim
      in
      failwith msg

  let conv_functions ~top_env (flambda : Flambda.program) =
    List.fold_left
      (fun decls (set_of_closures : Flambda.set_of_closures) ->
        let function_decls = set_of_closures.function_decls in
        let closure_functions = Variable.Map.keys function_decls.funs in
        Variable.Map.fold
          (fun var function_declaration decls ->
            let decl =
              conv_function_declaration ~top_env ~closure_functions var
                function_declaration
            in
            decl :: decls )
          function_decls.funs decls )
      []
      (Flambda_utils.all_sets_of_closures flambda)

  let block_type size : Type.descr =
    let fields = List.init size (fun _ -> ref_eq) in
    let sub : Type.Var.t =
      if size <= 1 then Gen_block else Block { size = size - 1 }
    in
    Struct { sub = Some sub; fields = (* Tag *)
                                      I8 :: (* size *)
                                            I16 :: fields }

  let block_float_type size : Type.descr =
    let fields = List.init size (fun _ -> Type.F64) in
    let sub : Type.Var.t option =
      if size = 0 then None else Some (BlockFloat { size = size - 1 })
    in
    Struct { sub; fields = (* size *) I16 :: fields }

  let gen_closure_type ~arity : Type.descr =
    let head : Type.atom list =
      if arity = 1 then
        [ I8 (* arity *); Rvar (Func { arity = 1 }) (* generic func *) ]
      else
        [ I8 (* arity *)
        ; Rvar (Func { arity = 1 }) (* generic func *)
        ; Rvar (Func { arity }) (* direct call func *)
        ]
    in
    Struct { sub = Some Env; fields = head }

  let closure_type ~arity ~fields : Type.descr =
    let head : Type.atom list =
      if arity = 1 then
        [ I8 (* arity *); Rvar (Func { arity = 1 }) (* generic func *) ]
      else
        [ I8 (* arity *)
        ; Rvar (Func { arity = 1 }) (* generic func *)
        ; Rvar (Func { arity }) (* direct call func *)
        ]
    in
    let fields = List.init fields (fun _ -> ref_eq) in
    Struct { sub = Some (Gen_closure { arity }); fields = head @ fields }

  let partial_closure_type ~arity ~applied : Type.descr =
    let args = List.init applied (fun _ -> ref_eq) in
    let fields : Type.atom list =
      [ Type.I8 (* arity *)
      ; Type.Rvar (Func { arity = 1 }) (* generic func *)
      ; Type.Rvar (Gen_closure { arity })
      ]
      @ args
    in
    Struct { sub = Some Env; fields }

  let func_type size : Type.descr =
    let params = List.init size (fun _ -> ref_eq) in
    Func { params = params @ [ Type.Rvar Env ]; result = Some ref_eq }

  let caml_curry_apply ~param_arg ~env_arg n =
    assert (n > 1);
    let partial_closure_arg_typ = Type.Var.Partial_closure (n, n - 1) in
    let partial_closure_var : Expr.Local.var = Partial_closure in
    let closure_arg_typ = Type.Var.Gen_closure { arity = n } in
    let closure_var : Expr.Local.var = Closure in
    let closure_args =
      let first_arg_field = 3 in
      List.init (n - 1) (fun i : Expr.t ->
          let field = first_arg_field + i in
          Unop
            ( Struct_get { typ = partial_closure_arg_typ; field }
            , Expr.Var (Expr.Local.V partial_closure_var) ) )
    in
    let args =
      closure_args @ [ Expr.Var param_arg; Expr.Var (V closure_var) ]
    in
    let func : Expr.t =
      Closure.get_direct_func (Expr.Var (Expr.Local.V closure_var)) ~arity:n
    in
    Expr.let_ partial_closure_var (Type.Rvar partial_closure_arg_typ)
      (Ref_cast { typ = partial_closure_arg_typ; r = Var env_arg })
      (Expr.let_ closure_var (Type.Rvar closure_arg_typ)
         (Unop
            ( Struct_get { typ = partial_closure_arg_typ; field = 2 }
            , Expr.Var (Expr.Local.V partial_closure_var) ) )
         (Expr.Call_ref { typ = Type.Var.Func { arity = n }; args; func }) )

  let caml_curry_alloc ~param_arg ~env_arg n m : Expr.t =
    (* arity, func, env, arg1..., argn-1, argn *)
    let closure_arg_typ = Type.Var.Partial_closure (n, m) in
    let closure_var : Expr.Local.var = Closure in
    let closure_local = Expr.Local.V closure_var in
    let closure_args =
      let first_arg_field = 3 in
      List.init m (fun i : Expr.t ->
          let field = first_arg_field + i in
          Unop
            (Struct_get { typ = closure_arg_typ; field }, Expr.Var closure_local) )
    in
    let closure_field =
      if m = 0 then
        Expr.Ref_cast
          { typ = Type.Var.Gen_closure { arity = n }; r = Var env_arg }
      else
        Expr.Unop
          ( Struct_get { typ = closure_arg_typ; field = 2 }
          , Expr.Var closure_local )
    in
    let fields =
      [ Expr.I32 1l; Expr.Ref_func (Caml_curry (n, m + 1)); closure_field ]
      @ closure_args @ [ Expr.Var param_arg ]
    in
    let body : Expr.t =
      Struct_new (Type.Var.Partial_closure (n, m + 1), fields)
    in
    if m = 0 then body
    else
      Expr.let_ closure_var (Type.Rvar closure_arg_typ)
        (Ref_cast { typ = closure_arg_typ; r = Var env_arg })
        body

  let caml_curry n m =
    let param_arg = Param.P ("arg", 0) in
    let env_arg = Param.Env in
    let body =
      if m = n - 1 then
        caml_curry_apply ~param_arg:(Expr.Local.Param param_arg)
          ~env_arg:(Expr.Local.Param env_arg) n
      else
        caml_curry_alloc ~param_arg:(Expr.Local.Param param_arg)
          ~env_arg:(Expr.Local.Param env_arg) n m
    in
    Func.Decl
      { params = [ (param_arg, ref_eq); (env_arg, Type.Rvar Type.Var.Env) ]
      ; body = Value (body, ref_eq)
      }

  let caml_apply n =
    (* TODO apply direct if right number of arguments *)
    let closure_param = Param.P ("closure", 0) in
    let param_i i = Param.P ("param", i) in
    let params = List.init n (fun i -> (param_i i, ref_eq)) in
    let rec build closure_var n params : Expr.t =
      let mk_call param =
        Expr.Call_ref
          { typ = Func { arity = 1 }
          ; args = [ Var (Param param); Var closure_var ]
          ; func = Closure.get_gen_func (Var closure_var)
          }
      in
      match params with
      | [] -> assert false
      | [ (param, _typ) ] -> mk_call param
      | (param, _typ) :: params ->
        let var : Expr.Local.var = Fresh ("partial_closure", n) in
        let call : Expr.t = Closure.cast (mk_call param) in
        let body = build (Expr.Local.V var) (n + 1) params in
        Let { var; typ = Rvar Env; defining_expr = call; body }
    in
    let body = build (Param closure_param) 0 params in
    Func.Decl
      { params = (closure_param, Type.Rvar Env) :: params
      ; body = Value (body, ref_eq)
      }

  let c_import (descr : Primitive.description) =
    let repr_type (t : Primitive.native_repr) : Type.atom =
      if descr.prim_native_name = "" then
        assert (t = Primitive.Same_as_ocaml_repr);
      match t with
      | Same_as_ocaml_repr -> ref_eq
      | Unboxed_float -> Type.F64
      | Unboxed_integer Pnativeint -> Type.I32
      | Unboxed_integer Pint32 -> Type.I32
      | Unboxed_integer Pint64 -> Type.I64
      | Untagged_int -> Type.I32
    in
    let params = List.map repr_type descr.prim_native_repr_args in
    let result = [ repr_type descr.prim_native_repr_res ] in
    Func.Import
      { params
      ; result
      ; module_ = "import"
      ; name = Func_id.prim_func_name descr
      }

  let runtime_import (descr : Runtime_import.t) =
    let params = List.init descr.arity (fun _ -> ref_eq) in
    let result = [ ref_eq ] in
    Func.Import { params; result; module_ = "runtime"; name = descr.name }

  let func_1_and_env =
    let env =
      let fields : Type.atom list =
        [ I8 (* arity *); Rvar (Func { arity = 1 }) (* generic func *) ]
      in
      (Type.Var.Env, Type.Struct { sub = None; fields })
    in
    let func_1 =
      let name = Type.Var.Func { arity = 1 } in
      let descr = func_type 1 in
      (name, descr)
    in
    Decl.Type_rec [ func_1; env ]

  let float_type =
    Decl.Type (Type.Var.Float, Type.Struct { sub = None; fields = [ F64 ] })

  let int32_type =
    Decl.Type (Type.Var.Int32, Type.Struct { sub = None; fields = [ I32 ] })

  let int64_type =
    Decl.Type (Type.Var.Int64, Type.Struct { sub = None; fields = [ I64 ] })

  let nativeint_type =
    Decl.Type (Type.Var.Nativeint, Type.Struct { sub = None; fields = [ I32 ] })

  let string_type =
    Decl.Type (Type.Var.String, Type.Array { sub = None; fields = I8 })

  let array_type =
    Decl.Type (Type.Var.Array, Type.Array { sub = None; fields = ref_eq })

  let floatarray_type =
    Decl.Type (Type.Var.FloatArray, Type.Array { sub = None; fields = F64 })

  let gen_block =
    let fields : Type.atom list = [ I8 (* tag *); I16 (* size *) ] in
    Decl.Type (Gen_block, Struct { sub = None; fields })

  let define_types_smaller ~max_size ~name ~descr ~decls =
    let sizes = List.init (max_size + 1) (fun i -> i) in
    List.fold_left
      (fun decls size ->
        let name = name size in
        let descr = descr size in
        Decl.Type (name, descr) :: decls )
      decls (List.rev sizes)

  let make_common () =
    let decls = [] in
    let decls =
      Arity.Set.fold
        (fun arity decls ->
          let ms = List.init arity (fun i -> i) in
          List.fold_left
            (fun decls applied_args ->
              let decl =
                Decl.Func
                  { name = Func_id.Caml_curry (arity, applied_args)
                  ; descr = caml_curry arity applied_args
                  }
              in
              decl :: decls )
            decls ms )
        (Arity.Set.remove 1 !State.arities)
        decls
    in
    let decls =
      Arity.Set.fold
        (fun arity decls ->
          let decl =
            Decl.Func
              { name = Func_id.Caml_apply arity; descr = caml_apply arity }
          in
          decl :: decls )
        !State.caml_applies decls
    in
    let decls =
      C_import.Set.fold
        (fun (descr : Primitive.description) decls ->
          let name = Func_id.prim_name descr in
          let descr = c_import descr in
          Decl.Func { name; descr } :: decls )
        !State.c_imports decls
    in
    let decls =
      Runtime_import.Set.fold
        (fun (descr : Runtime_import.t) decls ->
          let name = Func_id.Runtime descr.name in
          let descr = runtime_import descr in
          Decl.Func { name; descr } :: decls )
        !State.runtime_imports decls
    in
    let decls =
      define_types_smaller
        ~max_size:(Arity.Set.max_elt !State.block_sizes)
        ~name:(fun size -> Type.Var.Block { size })
        ~descr:block_type ~decls
    in
    let decls =
      define_types_smaller
        ~max_size:(Arity.Set.max_elt !State.block_float_sizes)
        ~name:(fun size -> Type.Var.BlockFloat { size })
        ~descr:block_float_type ~decls
    in
    let decls =
      Arity.Set.fold
        (fun arity decls ->
          let ms = List.init arity (fun i -> i) in
          List.fold_left
            (fun decls applied_args ->
              let decl =
                Decl.Type
                  ( Type.Var.Partial_closure (arity, applied_args)
                  , partial_closure_type ~arity ~applied:applied_args )
              in
              decl :: decls )
            decls ms )
        (Arity.Set.remove 1 !State.arities)
        decls
    in
    let decls =
      Closure_type.Set.fold
        (fun { arity; fields } decls ->
          let name = Type.Var.Closure { arity; fields } in
          let descr = closure_type ~arity ~fields in
          Decl.Type (name, descr) :: decls )
        !State.closure_types decls
    in
    let decls =
      Arity.Set.fold
        (fun arity decls ->
          let name = Type.Var.Gen_closure { arity } in
          let descr = gen_closure_type ~arity in
          Decl.Type (name, descr) :: decls )
        !State.arities decls
    in
    let decls = gen_block :: decls in
    let decls =
      Arity.Set.fold
        (fun arity decls ->
          let name = Type.Var.Func { arity } in
          let descr = func_type arity in
          Decl.Type (name, descr) :: decls )
        (Arity.Set.remove 1 !State.arities)
        decls
    in
    let decls =
      float_type :: int32_type :: int64_type :: nativeint_type :: string_type
      :: array_type :: floatarray_type :: func_1_and_env :: decls
    in
    decls
end

module ToWasm = struct
  module Cst = struct
    type t =
      | Int of int64
      | Float of float
      | String of string
      | Atom of string
      | Node of
          { name : string
          ; args_h : t list
          ; args_v : t list
          ; force_paren : bool
          }

    let print_lst f ppf l =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
        f ppf l

    let rec emit ppf = function
      | Int i -> Format.fprintf ppf "%Li" i
      | Float f -> Format.fprintf ppf "%h" f
      | String s -> Format.fprintf ppf "\"%s\"" s
      | Atom s -> Format.pp_print_string ppf s
      | Node { name; args_h; args_v; force_paren } -> begin
        match (args_h, args_v) with
        | [], [] ->
          if force_paren then Format.fprintf ppf "(%s)" name
          else Format.pp_print_string ppf name
        | _ ->
          Format.fprintf ppf "@[<v 2>@[<hov 2>";
          Format.fprintf ppf "(%s@ %a@]" name (print_lst emit) args_h;
          ( match args_v with
          | [] -> ()
          | _ -> Format.fprintf ppf "@ %a" (print_lst emit) args_v );
          Format.fprintf ppf ")@]"
      end

    let nodev name args =
      Node { name; args_h = []; args_v = args; force_paren = false }

    let nodehv name args_h args_v =
      Node { name; args_h; args_v; force_paren = false }

    let node name args =
      Node { name; args_h = args; args_v = []; force_paren = false }

    let node_p name args =
      Node { name; args_h = args; args_v = []; force_paren = true }

    let atom name = Atom name
  end

  module C = struct
    open Cst

    let ( !$ ) v = atom (Printf.sprintf "$%s" v)

    let type_name v = atom (Type.Var.name v)

    let global name typ descr = node "global" ([ !$name; typ ] @ descr)

    let reft name = node "ref" [ type_name name ]

    let struct_new_canon typ fields =
      let name =
        match mode with
        | Binarien -> "struct.new"
        | Reference -> "struct.new_canon"
      in
      node name (type_name typ :: fields)

    let array_new_canon_fixed typ size args =
      match mode with
      | Binarien -> node "array.new" ([ type_name typ ] @ args)
      | Reference ->
        node "array.new_canon_fixed"
          ([ type_name typ; Int (Int64.of_int size) ] @ args)

    let int i = Int (Int64.of_int i)

    let string s = String s

    let i32_ i = node "i32.const" [ int i ]

    let i32 i = node "i32.const" [ Int (Int64.of_int32 i) ]

    let i64 i = node "i64.const" [ Int i ]

    let f64 f = node "f64.const" [ Float f ]

    let i31_new i = node "i31.new" [ i ]

    let drop arg = node "drop" [ arg ]

    let drop' = atom "drop"

    let ref_func f = node "ref.func" [ !$(Func_id.name f) ]

    let global_get g = node "global.get" [ !$(Global.name g) ]

    let local_get l = node "local.get" [ !$(Expr.Local.name l) ]

    let local_set l arg = node "local.set" [ !$(Expr.Local.name l); arg ]

    let local_set' l = node "local.set" [ !$(Expr.Local.name l) ]

    let struct_get typ field arg =
      node "struct.get" [ type_name typ; int field; arg ]

    let sx_name (sx : Expr.sx) = match sx with S -> "s" | U -> "u"

    let struct_get_packed extend typ field arg =
      node
        (Printf.sprintf "struct.get_%s" (sx_name extend))
        [ type_name typ; int field; arg ]

    let struct_set typ field block value =
      node "struct.set" [ type_name typ; int field; block; value ]

    let call_ref typ args = node "call_ref" ([ type_name typ ] @ args)

    let call_ref' typ = node "call_ref" [ type_name typ ]

    let call func args = node "call" ([ !$(Func_id.name func) ] @ args)

    let ref_cast typ arg =
      let name =
        match mode with
        | Binarien -> "ref.cast_static"
        | Reference -> "ref.cast"
      in
      node name ([ type_name typ ] @ arg)

    let declare_func f =
      node "elem" [ atom "declare"; atom "func"; !$(Func_id.name f) ]

    let rec type_atom (t : Type.atom) =
      match t with
      | I8 -> atom "i8"
      | I16 -> atom "i16"
      | I32 -> atom "i32"
      | I64 -> atom "i64"
      | F64 -> atom "f64"
      | Rvar v -> reft v
      | Tuple l -> node "" (List.map type_atom l)

    let local l t = node "local" [ !$(Expr.Local.var_name l); type_atom t ]

    let param p t = node "param" [ !$(Param.name p); type_atom t ]

    let param_t t = node "param" [ type_atom t ]

    let result t = node "result" [ type_atom t ]

    let results t = node_p "result" (List.map type_atom t)

    let func ~name ~params ~result ~locals ~body =
      let fields = [ !$(Func_id.name name) ] @ params @ result @ locals in
      nodehv "func" fields body

    let field f = node "field" [ node "mut" [ type_atom f ] ]

    let struct_type fields = node "struct" (List.map field fields)

    let array_type f = node "array" [ node "mut" [ type_atom f ] ]

    let func_type ?name params res =
      let name =
        match name with None -> [] | Some name -> [ !$(Func_id.name name) ]
      in
      let res = List.map result res in
      node "func" (name @ List.map param_t params @ res)

    let if_then_else typ cond if_expr else_expr =
      let nopise e =
        match mode with
        | Reference -> e
        | Binarien -> ( match e with [] -> [ node_p "nop" [] ] | _ -> e )
      in
      let if_expr = nopise if_expr in
      let else_expr = nopise else_expr in
      node "if"
        [ results typ; cond; node_p "then" if_expr; node_p "else" else_expr ]

    let group_block result body = nodehv "block" [ results result ] body

    let block id result body =
      nodehv "block" [ !$(Block_id.name id); results result ] body

    let loop id result body =
      nodehv "loop" [ !$(Block_id.name id); results result ] body

    let br id args =
      match (mode, args) with
      | Binarien, _ :: _ :: _ ->
        node "br" [ !$(Block_id.name id); node "tuple.make" args ]
      | _ -> node "br" ([ !$(Block_id.name id) ] @ args)

    let br' id = node "br" [ !$(Block_id.name id) ]

    let br_on_cast id typ arg =
      match mode with
      | Binarien -> begin
        match typ with
        | Type.Var.I31 ->
          node "drop" [ node "br_on_i31" [ !$(Block_id.name id); arg ] ]
        | _ ->
          node "br_on_cast_static" [ !$(Block_id.name id); type_name typ; arg ]
      end
      | Reference ->
        node "br_on_cast" [ !$(Block_id.name id); type_name typ; arg ]

    let br_if id cond = node "br_if" [ !$(Block_id.name id); cond ]

    let br_table cond cases =
      node "br_table"
        (List.map (fun id -> !$(Block_id.name id)) cases @ [ cond ])

    let type_ name descr = node "type" [ type_name name; descr ]

    let sub name descr =
      match mode with
      | Binarien -> descr
      | Reference -> node "sub" [ type_name name; descr ]

    let tuple_make fields = node "tuple.make" fields

    let tuple_extract field tuple = node "tuple.extract" [ int field; tuple ]

    let rec_ l = node "rec" l

    let import module_ name e = node "import" [ String module_; String name; e ]

    let start f = node "start" [ !$(Func_id.name f) ]

    let module_ m = nodev "module" m
  end

  let option_to_list = function None -> [] | Some v -> [ v ]

  let tvar v = Type.Var.name v

  let gvar v = Global.name v

  let unit = Cst.node "i31.new" [ C.i32 0l ]

  let conv_binop (op : Expr.binop) args =
    match op with
    | I32_add -> Cst.node "i32.add" args
    | I32_sub -> Cst.node "i32.sub" args
    | I32_mul -> Cst.node "i32.mul" args
    | F64_add -> Cst.node "f64.add" args
    | F64_sub -> Cst.node "f64.sub" args
    | F64_mul -> Cst.node "f64.mul" args
    | F64_div -> Cst.node "f64.div" args
    | Ref_eq -> Cst.node "ref.eq" args

  let conv_nv_binop (op : Expr.nv_binop) block value =
    match op with
    | Struct_set { typ; field } -> [ C.struct_set typ field block value ]

  let conv_unop (op : Expr.unop) arg =
    match op with
    | I31_get_s -> Cst.node "i31.get_s" [ arg ]
    | I31_new -> Cst.node "i31.new" [ arg ]
    | Struct_get { typ; field } -> C.struct_get typ field arg
    | Struct_get_packed { typ; field; extend } ->
      C.struct_get_packed extend typ field arg
    | Ref_cast_i31 -> begin
      match mode with
      | Reference -> Cst.node "ref.cast" [ Cst.atom "i31"; arg ]
      | Binarien -> Cst.node "ref.as_i31" [ arg ]
    end
    | Is_i31 -> Cst.node "ref.is_i31" [ arg ]

  let nn_name (nn : Expr.nn) = match nn with S32 -> "32" | S64 -> "64"

  let sx_name (sx : Expr.sx) = match sx with S -> "s" | U -> "u"

  let irelop_name nn (op : Expr.irelop) =
    match op with
    | Eq -> Format.asprintf "i%s.eq" (nn_name nn)
    | Ne -> Format.asprintf "i%s.ne" (nn_name nn)
    | Lt sx -> Format.asprintf "i%s.lt_%s" (nn_name nn) (sx_name sx)
    | Gt sx -> Format.asprintf "i%s.gt_%s" (nn_name nn) (sx_name sx)
    | Le sx -> Format.asprintf "i%s.le_%s" (nn_name nn) (sx_name sx)
    | Ge sx -> Format.asprintf "i%s.ge_%s" (nn_name nn) (sx_name sx)

  let conv_irelop nn op a1 a2 = Cst.node (irelop_name nn op) [ a1; a2 ]

  let group e = match e with [ v ] -> v | _ -> C.group_block [ ref_eq ] e

  let rec conv_expr (expr : Expr.t) : Cst.t list =
    match expr with
    | Var v -> [ C.local_get v ]
    | Binop (op, (arg1, arg2)) ->
      [ conv_binop op [ conv_expr_group arg1; conv_expr_group arg2 ] ]
    | I_relop (nn, op, (arg1, arg2)) ->
      [ conv_irelop nn op (conv_expr_group arg1) (conv_expr_group arg2) ]
    | Unop (op, arg) -> [ conv_unop op (conv_expr_group arg) ]
    | Let { var; typ = _; defining_expr; body } ->
      C.local_set (Expr.Local.V var) (conv_expr_group defining_expr)
      :: conv_expr body
    | I32 i -> [ C.i32 i ]
    | I64 i -> [ C.i64 i ]
    | F64 f -> [ C.f64 f ]
    | Struct_new (typ, fields) ->
      let fields = List.map conv_expr_group fields in
      [ C.struct_new_canon typ fields ]
    | Array_new_fixed { typ; fields } ->
      let size = List.length fields in
      let fields = List.map conv_expr_group fields in
      [ C.array_new_canon_fixed typ size fields ]
    | Ref_func fid -> [ C.ref_func fid ]
    | Call_ref { typ; args; func } ->
      let args = List.map conv_expr_group args @ [ conv_expr_group func ] in
      [ C.call_ref typ args ]
    | Call { args; func } ->
      let args = List.map conv_expr_group args in
      [ C.call func args ]
    | Ref_cast { typ; r } -> [ C.ref_cast typ [ conv_expr_group r ] ]
    | Global_get g -> [ C.global_get g ]
    | Seq (effects, last) ->
      let effects = List.map conv_no_value effects in
      let last = conv_expr last in
      List.flatten effects @ last
    | If_then_else { cond; if_expr; else_expr } ->
      [ C.if_then_else [ ref_eq ] (conv_expr_group cond) (conv_expr if_expr)
          (conv_expr else_expr)
      ]
    | Let_cont { cont; params; handler; body } -> begin
      let result_types = List.map snd params in
      let fallthrough = Block_id.not_id cont in
      let body =
        C.block cont result_types [ C.br fallthrough [ conv_expr_group body ] ]
      in
      let handler_expr = conv_expr handler in
      match mode with
      | Reference ->
        let handler =
          List.map
            (fun (var, _typ) ->
              match var with
              | Some var -> C.local_set' (Expr.Local.V var)
              | None -> C.drop' )
            params
          @ handler_expr
        in
        [ C.block fallthrough [ ref_eq ] (body :: handler) ]
      | Binarien ->
        let set_locals =
          match params with
          | [] -> [ body ]
          | [ (None, _typ) ] -> [ C.drop body ]
          | [ (Some var, _typ) ] -> [ C.local_set (Expr.Local.V var) body ]
          | _ ->
            let local_tuple = Expr.Local.Block_result cont in
            let _i, assigns =
              List.fold_left
                (fun (i, assigns) (var, _typ) ->
                  match var with
                  | Some var ->
                    let project =
                      C.tuple_extract i (C.local_get (Expr.Local.V local_tuple))
                    in
                    let expr = C.local_set (Expr.Local.V var) project in
                    (i + 1, expr :: assigns)
                  | None -> (i + 1, assigns) )
                (0, []) params
            in
            [ C.local_set (Expr.Local.V local_tuple) body ] @ assigns
        in
        [ C.block fallthrough [ ref_eq ] (set_locals @ handler_expr) ]
    end
    | Apply_cont { cont; args } -> [ C.br cont (List.map conv_expr_group args) ]
    | Br_on_cast { value; typ; if_cast; if_else } ->
      [ C.br_on_cast if_cast typ (conv_expr_group value) ] @ conv_expr if_else
    | Br_if { cond; if_true; if_else } ->
      [ C.br_if if_true (conv_expr_group cond) ] @ conv_expr if_else
    | Br_table { cond; cases; default } ->
      [ C.br_table (conv_expr_group cond) (cases @ [ default ]) ]
    | Unit e -> conv_no_value e @ [ unit ]
    | NR nr -> conv_no_return nr

  and conv_expr_group e = group (conv_expr e)

  and conv_no_value (nv : Expr.no_value_expression) =
    match nv with
    | NV_seq effects ->
      let effects = List.map conv_no_value effects in
      List.flatten effects
    | NV_drop e -> [ C.drop (conv_expr_group e) ]
    | Assign { being_assigned; new_value } ->
      [ C.local_set (Expr.Local.V being_assigned) (conv_expr_group new_value) ]
    | NV_binop (op, (arg1, arg2)) ->
      conv_nv_binop op (conv_expr_group arg1) (conv_expr_group arg2)
    | Loop { cont; body } -> [ C.loop cont [] (conv_no_value body) ]
    | NV_if_then_else { cond; if_expr; else_expr } ->
      [ C.if_then_else [] (conv_expr_group cond) (conv_no_value if_expr)
          (conv_no_value else_expr)
      ]
    | NV_br_if { cond; if_true } -> [ C.br_if if_true (conv_expr_group cond) ]
    | NV -> []

  and conv_no_return (nr : Expr.no_return) =
    match nr with
    | NR_br_table { cond; cases; default } ->
      [ C.br_table (conv_expr_group cond) (cases @ [ default ]) ]
    | NR_let_cont { cont; params; handler; body } ->
      let result_types = List.map snd params in
      let body = conv_no_return body in
      let handler =
        List.map
          (fun (var, _typ) ->
            match var with
            | Some var -> C.local_set' (Expr.Local.V var)
            | None -> C.drop' )
          params
        @ conv_no_return handler
      in
      C.block cont result_types body :: handler
    | NR_if_then_else { cond; if_expr; else_expr } ->
      [ C.if_then_else [] (conv_expr_group cond) (conv_no_return if_expr)
          (conv_no_return else_expr)
      ]
    | NR_br { cont; arg } -> [ C.br cont [ conv_expr_group arg ] ]

  let conv_const name (const : Const.t) =
    match const with
    | Struct { typ; fields } ->
      let field (field : Const.field) : Cst.t =
        match field with
        | I8 i | I16 i -> C.i32_ i
        | I31 i -> C.i31_new (C.i32_ i)
        | Ref_func f -> C.ref_func f
        | Global g -> C.global_get g
      in
      C.global (Global.name name) (C.reft typ)
        [ C.struct_new_canon typ (List.map field fields) ]
    | Expr { typ; e } ->
      C.global (Global.name name) (C.type_atom typ) (conv_expr e)

  let conv_func name (func : Func.t) =
    match func with
    | Import { module_; name = prim_name; params; result } ->
      let typ = C.func_type ~name params result in
      [ C.import module_ prim_name typ ]
    | Decl { params; body } ->
      let func =
        let locals = Expr.required_locals body in
        let params = List.map (fun (p, t) -> C.param p t) params in
        let locals =
          Expr.Local.Map.fold (fun v t l -> C.local v t :: l) locals []
        in
        let body, result =
          match body with
          | Value (body, typ) -> (conv_expr body, [ C.result typ ])
          | No_value body -> (conv_no_value body, [])
        in
        C.func ~name ~params ~locals ~result ~body
      in
      [ C.declare_func name; func ]

  let conv_type name (descr : Type.descr) =
    match descr with
    | Struct { sub; fields } ->
      let descr = C.struct_type fields in
      let descr =
        match sub with None -> descr | Some sub -> C.sub sub descr
      in
      C.type_ name descr
    | Array { sub; fields } ->
      let descr = C.array_type fields in
      let descr =
        match sub with None -> descr | Some sub -> C.sub sub descr
      in
      C.type_ name descr
    | Func { params; result } ->
      C.type_ name (C.func_type params (option_to_list result))

  let conv_type_rec types =
    C.rec_ (List.map (fun (name, descr) -> conv_type name descr) types)

  let rec conv_decl = function
    | [] -> [ C.start Start ]
    | Decl.Const { name; descr } :: tl -> conv_const name descr :: conv_decl tl
    | Decl.Func { name; descr } :: tl ->
      let func = conv_func name descr in
      func @ conv_decl tl
    | Decl.Type (name, descr) :: tl ->
      let type_ = conv_type name descr in
      type_ :: conv_decl tl
    | Decl.Type_rec types :: tl ->
      let type_ = conv_type_rec types in
      type_ :: conv_decl tl

  let conv_module module_ = C.module_ (conv_decl module_)
end

let output_file ~output_prefix module_ =
  let wastfile = output_prefix ^ ".wast" in
  let oc = open_out_bin wastfile in
  let ppf = Format.formatter_of_out_channel oc in
  Misc.try_finally
    ~always:(fun () ->
      Format.fprintf ppf "@.";
      close_out oc )
    (* ~exceptionally:(fun () -> Misc.remove_file wastfile) *)
      (fun () -> ToWasm.Cst.emit ppf module_ )

let run ~output_prefix (flambda : Flambda.program) =
  State.reset ();
  let print_everything =
    match Sys.getenv_opt "WASMPRINT" with None -> false | Some _ -> true
  in
  let offsets = Wasm_closure_offsets.compute flambda in
  let top_env = Conv.{ offsets } in
  let m = Conv.conv_body top_env flambda.program_body [] in
  let closure_types = Conv.closure_types flambda in
  let functions = Conv.conv_functions ~top_env flambda in
  let m = closure_types @ m @ functions in
  if print_everything then
    Format.printf "WASM %s@.%a@." output_prefix Module.print m;
  let common = Conv.make_common () in
  if print_everything then Format.printf "COMMON@.%a@." Module.print common;
  let wasm = ToWasm.conv_module (common @ m) in
  Format.printf "@.%a@." ToWasm.Cst.emit wasm;
  output_file ~output_prefix wasm
