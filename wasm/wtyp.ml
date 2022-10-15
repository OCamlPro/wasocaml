[@@@ocaml.warning "-23"]

[@@@ocaml.warning "-37"]

[@@@ocaml.warning "-32"]

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

module State = struct
  let arities = ref Arity.Set.empty

  let block_sizes = ref Arity.Set.empty

  let closure_types = ref Closure_type.Set.empty

  let c_imports = ref C_import.Set.empty

  let add_arity (i : Arity.t) = Arity.Set.(arities += i)

  let add_block_size i = Arity.Set.(block_sizes += i)

  let add_closure_type ~arity ~fields =
    Closure_type.Set.(closure_types += { arity; fields })

  let add_c_import description = C_import.Set.(c_imports += description)

  let reset () =
    arities := Arity.Set.empty;
    block_sizes := Arity.Set.empty;
    closure_types := Closure_type.Set.empty;
    c_imports := C_import.Set.empty
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
      | Gen_block
      | I31
      | Float
      | Int32
      | Int64
      | Nativeint

    let name = function
      | V (name, n) -> Format.asprintf "$%s_%i" name n
      | Partial_closure (n, m) -> Format.asprintf "$Partial_closure_%i_%i" n m
      | Env -> Format.asprintf "$Env"
      | Func { arity } -> Format.asprintf "$Func_%i" arity
      | Block { size } -> Format.asprintf "$Block_%i" size
      | Gen_block -> Format.asprintf "$Gen_block"
      | Closure { arity; fields } ->
        Format.asprintf "$Closure_%i_%i" arity fields
      | Gen_closure { arity } -> Format.asprintf "$Gen_closure_%i" arity
      | Float -> "$Float"
      | Int32 -> "$Int32"
      | Int64 -> "$Int64"
      | Nativeint -> "$Nativeint"
      | I31 -> "i31"

    let print ppf v = Format.pp_print_string ppf (name v)
  end

  type atom =
    | I8
    | I16
    | I32
    | I64
    | F64
    | Any
    | Rvar of Var.t

  type descr =
    | Struct of
        { sub : Var.t option
        ; fields : atom list
        }
    | Func of
        { params : atom list
        ; result : atom option
        }

  let print_atom ppf = function
    | I8 -> Format.fprintf ppf "i8"
    | I16 -> Format.fprintf ppf "i16"
    | I32 -> Format.fprintf ppf "i32"
    | I64 -> Format.fprintf ppf "i64"
    | F64 -> Format.fprintf ppf "f64"
    | Any -> Format.fprintf ppf "ref_any"
    | Rvar v -> Format.fprintf ppf "ref_%a" Var.print v

  let print_descr ppf = function
    | Struct { sub; fields = atoms } ->
      let pp_sub ppf = function
        | None -> ()
        | Some sub -> Format.fprintf ppf "sub: %a;@ " Var.print sub
      in
      Format.fprintf ppf "@[<hov 2>Struct {%a%a}@]" pp_sub sub
        (print_list print_atom ";")
        atoms
    | Func { params; result = None } ->
      Format.fprintf ppf "@[<hov 2>Func {%a}@]"
        (print_list print_atom ",")
        params
    | Func { params; result = Some result } ->
      Format.fprintf ppf "@[<hov 2>Func {%a} ->@ %a@]"
        (print_list print_atom ",")
        params print_atom result
end

module Func_id = struct
  type t =
    | V of string * int
    | Symbol of Symbol.t
    | Caml_curry of int * int
    | C_import of string
    | Start

  let name = function
    | V (name, n) -> Format.asprintf "%s_%i" name n
    | Symbol s -> Format.asprintf "%a" Symbol.print s
    | Caml_curry (n, m) ->
      if m = 0 then Format.asprintf "Caml_curry_%i" n
      else Format.asprintf "Caml_curry_%i_%i" n m
    | C_import s -> Format.asprintf "C_%s" s
    | Start -> "Start"

  let print ppf = function
    | V (name, n) -> Format.fprintf ppf "%s_%i" name n
    | Symbol s -> Symbol.print ppf s
    | Caml_curry (n, m) ->
      if m = 0 then Format.fprintf ppf "Caml_curry_%i" n
      else Format.fprintf ppf "Caml_curry_%i_%i" n m
    | C_import s -> Format.fprintf ppf "C_%s" s
    | Start -> Format.pp_print_string ppf "Start"

  let of_var_closure_id var =
    let name, id = Variable.unique_name_id var in
    V (name, id)

  let of_closure_id closure_id =
    let var = Closure_id.unwrap closure_id in
    of_var_closure_id var
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
    S name
end

module Expr = struct
  module Local = struct
    type var = string * int

    let print_var ppf (name, n) = Format.fprintf ppf "L(%s_%i)" name n

    let var_name (name, n) = Format.asprintf "%s_%i" name n

    type t =
      | V of var
      | Param of Param.t

    let print ppf = function
      | V v -> print_var ppf v
      | Param p -> Param.print ppf p

    let var_of_var v = Variable.unique_name_id v

    let of_var v = V (var_of_var v)

    let name = function V v -> var_name v | Param p -> Param.name p

    module M = struct
      type nonrec t = var

      let compare = compare
    end

    module Map = Map.Make (M)
  end

  type binop =
    | I32_add
    | I32_sub
    | I32_mul
    | Struct_set of
        { typ : Type.Var.t
        ; field : int
        }

  type unop =
    | I31_get_s
    | I31_new
    | Drop
    | Struct_get of
        { typ : Type.Var.t
        ; field : int
        }

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
    | Binop of binop * (t * t)
    | Unop of unop * t
    | Struct_new of Type.Var.t * t list
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
    | Seq of t list

  let print_binop ppf = function
    | I32_add -> Format.fprintf ppf "I32_add"
    | I32_sub -> Format.fprintf ppf "I32_sub"
    | I32_mul -> Format.fprintf ppf "I32_mul"
    | Struct_set { typ; field } ->
      Format.fprintf ppf "@[<hov 2>Struct_set(%a).(%i)@]" Type.Var.print typ
        field

  let print_unop ppf = function
    | I31_get_s -> Format.fprintf ppf "I31_get_s"
    | I31_new -> Format.fprintf ppf "I31_new"
    | Drop -> Format.fprintf ppf "Drop"
    | Struct_get { typ; field } ->
      Format.fprintf ppf "@[<hov 2>Struct_get(%a).(%i)@]" Type.Var.print typ
        field

  let rec print ppf = function
    | Var l -> Local.print ppf l
    | I32 i -> Format.fprintf ppf "%li" i
    | I64 i -> Format.fprintf ppf "%Li" i
    | F64 f -> Format.fprintf ppf "%g" f
    | Ref_func f -> Format.fprintf ppf "Ref_func %a" Func_id.print f
    | Let { var; defining_expr; body } ->
      Format.fprintf ppf "@[<hov 2>Let %a =@ %a@]@ in@ %a" Local.print_var var
        print defining_expr print body
    | Binop (binop, (arg1, arg2)) ->
      Format.fprintf ppf "@[<hov 2>Binop(%a:@ %a,@ %a)@]" print_binop binop
        print arg1 print arg2
    | Unop (unop, arg) ->
      Format.fprintf ppf "@[<hov 2>Unop(%a:@ %a)@]" print_unop unop print arg
    | Struct_new (typ, args) ->
      Format.fprintf ppf "@[<hov 2>Struct_new(%a:@ %a)@]" Type.Var.print typ
        (print_list print ",") args
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
    | Seq l -> Format.fprintf ppf "@[<v 2>Seq(%a)@]" (print_list print ";") l

  let let_ var typ defining_expr body = Let { var; typ; defining_expr; body }

  let required_locals expr =
    let rec loop acc = function
      | Var _ | I32 _ | I64 _ | F64 _ | Ref_func _ -> acc
      | Let { var; typ; defining_expr; body } ->
        let acc = Local.Map.add var typ acc in
        let acc = loop acc defining_expr in
        loop acc body
      | Binop (_op, (arg1, arg2)) ->
        let acc = loop acc arg1 in
        loop acc arg2
      | Unop (_op, arg) -> loop acc arg
      | Struct_new (_typ, args) ->
        List.fold_left (fun acc arg -> loop acc arg) acc args
      | Call_ref { typ = _; args; func } ->
        List.fold_left (fun acc arg -> loop acc arg) (loop acc func) args
      | Call { args; func = _ } ->
        List.fold_left (fun acc arg -> loop acc arg) acc args
      | Ref_cast { typ = _; r } -> loop acc r
      | Global_get _ -> acc
      | Seq l -> List.fold_left (fun acc arg -> loop acc arg) acc l
    in
    loop Local.Map.empty expr
end

module Func = struct
  type t =
    | Decl of
        { params : (Param.t * Type.atom) list
        ; result : Type.atom option
        ; body : Expr.t
        }
    | Import of
        { params : Type.atom list
        ; result : Type.atom list
        ; module_ : string
        ; name : string
        }

  let print ppf = function
    | Decl { params; result; body } ->
      let pr_result ppf = function
        | None -> ()
        | Some result -> Format.fprintf ppf " -> %a" Type.print_atom result
      in
      let param ppf (p, typ) =
        Format.fprintf ppf "(%a: %a)" Param.print p Type.print_atom typ
      in
      Format.fprintf ppf "@[<hov 2>Func (%a)%a@ {@ %a@ }@]"
        (print_list param ",") params pr_result result Expr.print body
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
  type env =
    { bound_vars : Variable.Set.t
    ; params : Variable.Set.t
    ; closure_vars : Variable.Set.t
    }

  let empty_env =
    { bound_vars = Variable.Set.empty
    ; params = Variable.Set.empty
    ; closure_vars = Variable.Set.empty
    }

  let enter_function ~params ~free_vars =
    let params =
      List.fold_left
        (fun params p -> Variable.Set.add (Parameter.var p) params)
        Variable.Set.empty params
    in
    let closure_vars = Variable.Set.diff free_vars params in
    { bound_vars = Variable.Set.empty; params; closure_vars }

  let const_float f : Expr.t = Struct_new (Float, [ F64 f ])

  let const_int32 i : Expr.t = Struct_new (Int32, [ I32 i ])

  let const_int64 i : Expr.t = Struct_new (Int64, [ I64 i ])

  let const_nativeint i : Expr.t =
    Struct_new (Nativeint, [ I32 (Nativeint.to_int32 i) ])

  let bind_var env var =
    { env with bound_vars = Variable.Set.add var env.bound_vars }

  let conv_var (env : env) (var : Variable.t) : Expr.t =
    begin
      if Variable.Set.mem var env.params then
        Var (Expr.Local.Param (Param.of_var var))
      else if Variable.Set.mem var env.bound_vars then
        Var (Expr.Local.of_var var)
      else if Variable.Set.mem var env.closure_vars then
        failwith "TODO closure_var"
      else Misc.fatal_errorf "Unbound variable %a" Variable.print var
    end

  let dummy_const = 123456789

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

  let cast_to_env r = Expr.Ref_cast { typ = Type.Var.Env; r }

  let conv_apply env (apply : Flambda.apply) : Expr.t =
    match apply.kind with
    | Indirect -> failwith "TODO call indirect"
    | Direct closure_id ->
      let func = Func_id.of_closure_id closure_id in
      let args =
        List.map (conv_var env) apply.args
        @ [ cast_to_env (conv_var env apply.func) ]
      in
      Call { func; args }

  let conv_allocated_const (const : Allocated_const.t) : Const.t =
    match const with
    | Float f -> Expr { typ = Rvar Float; e = const_float f }
    | Int32 i -> Expr { typ = Rvar Int32; e = const_int32 i }
    | Int64 i -> Expr { typ = Rvar Int64; e = const_int64 i }
    | Nativeint i -> Expr { typ = Rvar Nativeint; e = const_nativeint i }
    | Float_array _ | Immutable_float_array _ | String _ | Immutable_string _ ->
      failwith
        (Format.asprintf "TODO allocated const %a" Allocated_const.print const)

  let rec conv_body (expr : Flambda.program_body) effects : Module.t =
    match expr with
    | Let_symbol (symbol, Set_of_closures set, body) ->
      let decl = closed_function_declarations symbol set.function_decls in
      let body = conv_body body effects in
      decl @ body
    | Let_symbol (symbol, const, body) ->
      let decls, new_effects =
        conv_symbol ~symbols_being_bound:Symbol.Set.empty symbol const
      in
      assert (new_effects = []);
      let body = conv_body body effects in
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
      let body = conv_body body effects in
      decls @ body
    | Initialize_symbol (symbol, tag, fields, body) ->
      let decl, effect = conv_initialize_symbol symbol tag fields in
      decl :: conv_body body (effect @ effects)
    | Effect (expr, body) ->
      let effect : Expr.t = Unop (Drop, conv_expr empty_env expr) in
      conv_body body (effect :: effects)
    | End _end_symbol ->
      [ Decl.Func
          { name = Start
          ; descr =
              Decl { params = []; result = None; body = Seq (List.rev effects) }
          }
      ]

  and conv_initialize_symbol symbol tag fields =
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
            let expr = conv_expr empty_env expr in
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
    let effect (field, expr) : Expr.t =
      let typ = Type.Var.Block { size } in
      Binop
        ( Struct_set { typ; field = field + 2 }
        , (Global_get (Global.of_symbol symbol), expr) )
    in
    let effect = List.map effect !fields_to_update in
    (decl, effect)

  and conv_symbol ~symbols_being_bound symbol
      (const : Flambda.constant_defining_value) : Decl.t list * Expr.t list =
    match const with
    | Block (tag, fields) ->
      let name = Global.of_symbol symbol in
      let descr, fields_to_update =
        const_block ~symbols_being_bound tag fields
      in
      let new_effects =
        List.map
          (fun (field_to_update, field_contents) : Expr.t ->
            let typ = Type.Var.Block { size = field_to_update + 1 } in
            Binop
              ( Struct_set { typ; field = field_to_update + 2 }
              , ( Global_get (Global.of_symbol symbol)
                , Global_get (Global.of_symbol field_contents) ) ) )
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
      (fun name declaration declarations ->
        let function_name = Func_id.of_var_closure_id name in
        let e, closure =
          closed_function_declaration function_name declaration
        in
        (* let closure_name = Global.Closure name in *)
        let closure_name =
          let closure_symbol =
            Compilenv.closure_symbol (Closure_id.wrap name)
          in
          Global.of_symbol closure_symbol
        in
        Decl.Func { name = function_name; descr = e }
        :: Decl.Const { name = closure_name; descr = closure }
        :: declarations )
      declarations.funs []

  and closed_function_declaration function_name
      (declaration : Flambda.function_declaration) : Func.t * Const.t =
    let arity = List.length declaration.params in
    let params =
      List.map
        (fun p -> (Param.of_var (Parameter.var p), Type.Any))
        declaration.params
    in
    let env =
      enter_function ~params:declaration.params
        ~free_vars:declaration.free_variables
    in
    let body = conv_expr env declaration.body in
    let func =
      Func.Decl
        { params = params @ [ (Param.Env, Type.Rvar Type.Var.Env) ]
        ; result = Some Type.Any
        ; body
        }
    in
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
    (func, closure)

  and conv_expr env (expr : Flambda.t) : Expr.t =
    match expr with
    | Let { var; defining_expr; body; _ } ->
      let local = Expr.Local.var_of_var var in
      let defining_expr = conv_named env defining_expr in
      let body = conv_expr (bind_var env var) body in
      Let { var = local; typ = Type.Any; defining_expr; body }
    | Var var -> conv_var env var
    | Apply apply -> conv_apply env apply
    (* | If _ -> I *)
    | _ ->
      let msg = Format.asprintf "TODO (conv_expr) %a" Flambda.print expr in
      failwith msg

  and conv_named env (named : Flambda.named) : Expr.t =
    match named with
    | Prim (prim, args, _dbg) -> conv_prim env ~prim ~args
    | Symbol s -> Global_get (Global.of_symbol s)
    | Expr (Var var) -> conv_var env var
    | Const c ->
      let c = match c with Int i -> i | Char c -> Char.code c in
      Unop (I31_new, I32 (Int32.of_int c))
    | Expr e -> conv_expr env e
    | Read_symbol_field (symbol, field) ->
      let typ = Type.Var.Block { size = field + 1 } in
      Unop
        ( Struct_get { typ; field = field + 2 }
        , Global_get (Global.of_symbol symbol) )
    | _ ->
      let msg = Format.asprintf "TODO named %a" Flambda.print_named named in
      failwith msg

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
    let i32 v = Expr.Unop (I31_get_s, Ref_cast { typ = I31; r = v }) in
    let i31 v = Expr.Unop (I31_new, v) in
    match prim with
    | Paddint -> i31 (Expr.Binop (I32_add, args2 (List.map i32 args)))
    | Psubint -> i31 (Expr.Binop (I32_sub, args2 (List.map i32 args)))
    | Pmulint -> i31 (Expr.Binop (I32_mul, args2 (List.map i32 args)))
    | Pccall descr ->
      State.add_c_import descr;
      Call { args; func = Func_id.C_import descr.prim_name }
    | Pmakeblock (tag, _mut, _shape) ->
      let size = List.length args in
      Struct_new
        ( Block { size }
        , I32 (Int32.of_int tag) :: I32 (Int32.of_int size) :: args )
    | Pfield field ->
      let arg = arg1 args in
      let typ : Type.Var.t = Block { size = field + 1 } in
      Unop (Struct_get { typ; field = field + 2 }, Ref_cast { typ; r = arg })
    | _ ->
      let msg =
        Format.asprintf "TODO prim %a" Printclambda_primitives.primitive prim
      in
      failwith msg

  let block_type size : Type.descr =
    let fields = List.init size (fun _ -> Type.Any) in
    let sub : Type.Var.t =
      if size = 1 then Gen_block else Block { size = size - 1 }
    in
    Struct { sub = Some sub; fields = (* Tag *)
                                      I8 :: (* size *)
                                            I16 :: fields }

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
    let fields = List.init fields (fun _ -> Type.Any) in
    Struct { sub = Some (Gen_closure { arity }); fields = head @ fields }

  let partial_closure_type ~arity ~applied : Type.descr =
    let args = List.init applied (fun _ -> Type.Any) in
    let fields : Type.atom list =
      [ Type.I8 (* arity *)
      ; Type.Rvar (Func { arity = 1 }) (* generic func *)
      ; Type.Rvar (Gen_closure { arity })
      ]
      @ args
    in
    Struct { sub = Some Env; fields }

  let func_type size : Type.descr =
    let params = List.init size (fun _ -> Type.Any) in
    Func { params = params @ [ Type.Rvar Env ]; result = Some Any }

  let caml_curry_apply ~param_arg ~env_arg n =
    let partial_closure_arg_typ = Type.Var.Partial_closure (n, n - 1) in
    let partial_closure_var : Expr.Local.var = ("partial_closure", 0) in
    let closure_arg_typ = Type.Var.Gen_closure { arity = n } in
    let closure_var : Expr.Local.var = ("closure", 0) in
    let closure_args =
      let first_arg_field = 3 in
      List.init (n - 1) (fun i : Expr.t ->
          let field = first_arg_field + i in
          Unop
            ( Struct_get { typ = partial_closure_arg_typ; field }
            , Expr.Var (Expr.Local.V partial_closure_var) ) )
    in
    let args = closure_args @ [ Expr.Var param_arg; Expr.Var env_arg ] in
    let func : Expr.t =
      Unop
        ( Struct_get { typ = closure_arg_typ; field = 2 }
        , Expr.Var (Expr.Local.V closure_var) )
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
    let closure_var : Expr.Local.var = ("closure", 0) in
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
      { params = [ (param_arg, Type.Any); (env_arg, Type.Rvar Type.Var.Env) ]
      ; result = Some Type.Any
      ; body
      }

  let c_import (descr : Primitive.description) =
    let repr_type (t : Primitive.native_repr) : Type.atom =
      match t with
      | Same_as_ocaml_repr -> Type.Any
      | Unboxed_float -> Type.F64
      | Unboxed_integer Pnativeint -> Type.I32
      | Unboxed_integer Pint32 -> Type.I32
      | Unboxed_integer Pint64 -> Type.I64
      | Untagged_int -> Type.I32
    in
    let params = List.map repr_type descr.prim_native_repr_args in
    let result = [ repr_type descr.prim_native_repr_res ] in
    Func.Import { params; result; module_ = "import"; name = descr.prim_name }

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

  let gen_block =
    let fields : Type.atom list = [ I8 (* tag *); I16 (* size *) ] in
    Decl.Type (Gen_block, Struct { sub = None; fields })

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
      C_import.Set.fold
        (fun (descr : Primitive.description) decls ->
          let name = Func_id.C_import descr.prim_name in
          let descr = c_import descr in
          Decl.Func { name; descr } :: decls )
        !State.c_imports decls
    in
    let decls =
      let max_block_size = Arity.Set.max_elt !State.block_sizes in
      let block_sizes = List.init max_block_size (fun i -> i + 1) in
      List.fold_left
        (fun decls size ->
          let name = Type.Var.Block { size } in
          let descr = block_type size in
          Decl.Type (name, descr) :: decls )
        decls (List.rev block_sizes)
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
      float_type :: int32_type :: int64_type :: nativeint_type :: func_1_and_env
      :: decls
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
      node "struct.new_canon" (type_name typ :: fields)

    let int i = Int (Int64.of_int i)

    let string s = String s

    let i32_ i = node "i32.const" [ int i ]

    let i32 i = node "i32.const" [ Int (Int64.of_int32 i) ]

    let i64 i = node "i64.const" [ Int i ]

    let f64 f = node "f64.const" [ Float f ]

    let i31_new i = node "i31.new" [ i ]

    let ref_func f = node "ref.func" [ !$(Func_id.name f) ]

    let global_get g = node "global.get" [ !$(Global.name g) ]

    let local_get l = node "local.get" [ !$(Expr.Local.name l) ]

    let local_set l = node "local.set" [ !$(Expr.Local.name l) ]

    let struct_get typ field = node "struct.get" [ type_name typ; int field ]

    let struct_set typ field = node "struct.set" [ type_name typ; int field ]

    let call_ref typ = node "call_ref" [ type_name typ ]

    let call func = node "call" [ !$(Func_id.name func) ]

    let ref_cast typ = node "ref.cast" [ type_name typ ]

    let declare_func f =
      node "elem" [ atom "declare"; atom "func"; !$(Func_id.name f) ]

    let type_atom (t : Type.atom) =
      match t with
      | I8 -> atom "i8"
      | I16 -> atom "i16"
      | I32 -> atom "i32"
      | I64 -> atom "i64"
      | F64 -> atom "f64"
      | Any -> node "ref" [ atom "any" ]
      | Rvar v -> reft v

    let local l t = node "local" [ !$(Expr.Local.var_name l); type_atom t ]

    let param p t = node "param" [ !$(Param.name p); type_atom t ]

    let param_t t = node "param" [ type_atom t ]

    let result t = node "result" [ type_atom t ]

    let func ~name ~params ~result ~locals ~body =
      let fields = [ !$(Func_id.name name) ] @ params @ result @ locals in
      nodehv "func" fields body

    let field f = node "field" [ node "mut" [ type_atom f ] ]

    let struct_type fields = node "struct" (List.map field fields)

    let func_type ?name params res =
      let name =
        match name with None -> [] | Some name -> [ !$(Func_id.name name) ]
      in
      let res = List.map result res in
      node "func" (name @ List.map param_t params @ res)

    let type_ name descr = node "type" [ type_name name; descr ]

    let sub name descr = node "sub" [ type_name name; descr ]

    let rec_ l = node "rec" l

    let import module_ name e = node "import" [ String module_; String name; e ]

    let start f = node "start" [ !$(Func_id.name f) ]

    let module_ m = nodev "module" m
  end

  let option_to_list = function None -> [] | Some v -> [ v ]

  let tvar v = Type.Var.name v

  let gvar v = Global.name v

  let conv_binop = function
    | Expr.I32_add -> Cst.atom "i32.add"
    | Expr.I32_sub -> Cst.atom "i32.sub"
    | Expr.I32_mul -> Cst.atom "i32.mul"
    | Expr.Struct_set { typ; field } -> C.struct_set typ field

  let conv_unop = function
    | Expr.I31_get_s -> Cst.atom "i31.get_s"
    | Expr.I31_new -> Cst.atom "i31.new"
    | Expr.Drop -> Cst.atom "drop"
    | Expr.Struct_get { typ; field } -> C.struct_get typ field

  let rec conv_expr (expr : Expr.t) =
    match expr with
    | Var v -> [ C.local_get v ]
    | Binop (op, (arg1, arg2)) ->
      conv_expr arg1 @ conv_expr arg2 @ [ conv_binop op ]
    | Unop (op, arg) -> conv_expr arg @ [ conv_unop op ]
    | Let { var; typ = _; defining_expr; body } ->
      conv_expr defining_expr
      @ (C.local_set (Expr.Local.V var) :: conv_expr body)
    | I32 i -> [ C.i32 i ]
    | I64 i -> [ C.i64 i ]
    | F64 f -> [ C.f64 f ]
    | Struct_new (typ, fields) ->
      let fields = List.map conv_expr fields in
      List.flatten fields @ [ C.struct_new_canon typ [] ]
    | Ref_func fid -> [ C.ref_func fid ]
    | Call_ref { typ; args; func } ->
      List.flatten (List.map conv_expr args)
      @ conv_expr func
      @ [ C.call_ref typ ]
    | Call { args; func } ->
      List.flatten (List.map conv_expr args) @ [ C.call func ]
    | Ref_cast { typ; r } -> conv_expr r @ [ C.ref_cast typ ]
    | Global_get g -> [ C.global_get g ]
    | Seq l -> List.flatten (List.map conv_expr l)

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
    | Decl { params; result; body } ->
      let func =
        let locals = Expr.required_locals body in
        let params = List.map (fun (p, t) -> C.param p t) params in
        let locals =
          Expr.Local.Map.fold (fun v t l -> C.local v t :: l) locals []
        in
        let body = conv_expr body in
        let result =
          match result with None -> [] | Some typ -> [ C.result typ ]
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
  let m = Conv.conv_body flambda.program_body [] in
  if print_everything then
    Format.printf "WASM %s@.%a@." output_prefix Module.print m;
  let common = Conv.make_common () in
  if print_everything then Format.printf "COMMON@.%a@." Module.print common;
  let wasm = ToWasm.conv_module (common @ m) in
  Format.printf "@.%a@." ToWasm.Cst.emit wasm;
  output_file ~output_prefix wasm
