open Wstate
module Type = Wtype

[@@@ocaml.warning "-23"]
[@@@ocaml.warning "-37"]
[@@@ocaml.warning "-32"]

let ignore_unimplemented = true

let print_list f sep ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "%s@ " sep)
    f ppf l

let ref_eq = Type.Rvar Eq

open Wident
open Wmodule

module Conv = struct
  type top_env = { offsets : Wasm_closure_offsets.t }

  type env =
    { bound_vars : Variable.Set.t
    ; params : Variable.Set.t
    ; closure_vars : Variable.Set.t
    ; closure_functions : Variable.Set.t
    ; mutables : Mutable_variable.Set.t
    ; current_function : Closure_id.t option
    ; top_env : top_env
    ; catch : Block_id.t Static_exception.Map.t
    ; constant_function : bool
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
    ; constant_function = false
    }

  let enter_function ~top_env ~closure_id ~params ~free_vars ~closure_functions
      ~constant_function =
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
    ; constant_function
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

    let closure_info top_env closure_id =
      let unit = Closure_id.get_compilation_unit closure_id in
      if Compilation_unit.is_current unit then (
        try Closure_id.Map.find closure_id top_env.offsets.function_accessors
        with Not_found as e ->
          Format.printf "Missing closure_id %a in current compilation unit"
            Closure_id.print closure_id;
          raise e )
      else
        let export_info = Compilenv.approx_env () in
        try
          Closure_id.Map.find closure_id
            export_info.wasm_offsets.function_accessors
        with Not_found as e ->
          Format.printf "Missing closure_id %a in imported compilation units"
            Closure_id.print closure_id;
          raise e

    let var_within_closure_info top_env var_within_closure =
      let unit = Var_within_closure.get_compilation_unit var_within_closure in
      if Compilation_unit.is_current unit then (
        try
          Var_within_closure.Map.find var_within_closure
            top_env.offsets.free_variable_accessors
        with Not_found as e ->
          Format.printf
            "Missing var_within_closure %a in current compilation unit"
            Var_within_closure.print var_within_closure;
          raise e )
      else
        let export_info = Compilenv.approx_env () in
        try
          Var_within_closure.Map.find var_within_closure
            export_info.wasm_offsets.free_variable_accessors
        with Not_found as e ->
          Format.printf
            "Missing var_within_closure %a in imported compilation units"
            Var_within_closure.print var_within_closure;
          raise e

    let project_closure ?(cast : unit option) (top_env : top_env) closure_id
        set_of_closures : Expr.t =
      let accessor = closure_info top_env closure_id in
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
      let accessor = var_within_closure_info top_env var in
      let closure_info = closure_info top_env closure_id in
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
    let get_tag ~(cast : bool) e : Expr.t =
      let block : Expr.t =
        match cast with
        | false -> e
        | true -> Ref_cast { r = e; typ = Gen_block }
      in
      Unop (Struct_get_packed { extend = U; typ = Gen_block; field = 0 }, block)

    let get_field ~(cast : bool) e ~field : Expr.t =
      let size = field + 1 in
      State.add_block_size size;
      let typ : Type.Var.t = Block { size } in
      let e =
        match cast with false -> e | true -> Expr.(Ref_cast { typ; r = e })
      in
      Unop (Struct_get { typ; field = field + 2 }, e)

    let set_field ~(cast : bool) ~block value ~field : Expr.no_value_expression
        =
      let size = field + 1 in
      State.add_block_size size;
      let typ : Type.Var.t = Block { size } in
      let block =
        match cast with
        | false -> block
        | true -> Expr.(Ref_cast { typ; r = block })
      in
      NV_binop (Struct_set { typ; field = field + 2 }, (block, value))
  end

  module FloatBlock = struct
    let make fields : Expr.t =
      let size = List.length fields in
      State.add_block_float_size size;
      Struct_new (BlockFloat { size }, Expr.I32 (Int32.of_int size) :: fields)

    let get_field e ~field : Expr.t =
      let size = field + 1 in
      State.add_block_float_size size;
      let typ : Type.Var.t = BlockFloat { size } in
      Unop
        (Struct_get { typ; field = field + 1 }, Expr.(Ref_cast { typ; r = e }))

    let set_field ~block value ~field : Expr.no_value_expression =
      let size = field + 1 in
      State.add_block_float_size size;
      let typ : Type.Var.t = BlockFloat { size } in
      NV_binop
        ( Struct_set { typ; field = field + 1 }
        , (Expr.(Ref_cast { typ; r = block }), value) )
  end

  let integer_comparision (c : Lambda.integer_comparison) : Expr.irelop =
    match c with
    | Ceq -> Eq
    | Cne -> Ne
    | Clt -> Lt S
    | Cgt -> Gt S
    | Cle -> Le S
    | Cge -> Ge S

  module WSymbol = struct
    let record symbol =
      if not (Compilation_unit.is_current (Symbol.compilation_unit symbol)) then
        State.add_global_import symbol

    let get symbol =
      record symbol;
      Expr.Global_get (Global.Sym symbol)

    let const symbol =
      record symbol;
      Const.Global (Global.Sym symbol)

    let export_name symbol = Linkage_name.to_string @@ Symbol.label symbol
  end

  module WInt = struct
    let untag e : Expr.t = Unop (I31_get_s, Unop (Ref_cast_i31, e))

    let tag e : Expr.t = Unop (I31_new, e)
  end

  module WBint = struct
    let box_type (t : Primitive.boxed_integer) : Type.Var.t =
      match t with Pint32 -> Int32 | Pint64 -> Int64 | Pnativeint -> Nativeint

    let size (t : Primitive.boxed_integer) : Expr.nn =
      match t with Pint32 -> S32 | Pint64 -> S64 | Pnativeint -> S32

    let unboxed_const (t : Primitive.boxed_integer) n : Expr.t =
      match size t with
      | S32 -> I32 (Int32.of_int n)
      | S64 -> I64 (Int64.of_int n)

    let conv ~from ~to_ e : Expr.t =
      match (size from, size to_) with
      | S32, S32 | S64, S64 -> e
      | S32, S64 ->
          Unop (I64_extend_i32 S, e)
      | S64, S32 ->
          Unop (I32_wrap_i64, e)
      (* | sfrom, sto ->
       *   Unop (Reinterpret { from_type = I sfrom; to_type = I sto }, e) *)

    let unbox t e : Expr.t =
      let typ = box_type t in
      Unop (Struct_get { typ; field = 0 }, Ref_cast { typ; r = e })

    let box t e : Expr.t =
      let typ = box_type t in
      Struct_new (typ, [ e ])

    let binop (t : Primitive.boxed_integer) op (a1, a2) : Expr.t =
      let args = (unbox t a1, unbox t a2) in
      box t (Binop (I_binop (op, size t), args))

    let binop_with_int (t : Primitive.boxed_integer) op (a1, a2) : Expr.t =
      let args = (unbox t a1, WInt.untag a2) in
      box t (Binop (I_binop (op, size t), args))

    let relop (t : Primitive.boxed_integer) op (a1, a2) : Expr.t =
      let args = (unbox t a1, unbox t a2) in
      WInt.tag (I_relop (size t, integer_comparision op, args))
  end

  type is_not =
    | Not
    | Id

  let float_comparision (c : Lambda.float_comparison) : Expr.frelop * is_not =
    match c with
    | CFeq -> (Eq, Id)
    | CFneq -> (Eq, Not)
    | CFlt -> (Lt, Id)
    | CFnlt -> (Lt, Not)
    | CFgt -> (Gt, Id)
    | CFngt -> (Gt, Not)
    | CFle -> (Le, Id)
    | CFnle -> (Le, Not)
    | CFge -> (Ge, Id)
    | CFnge -> (Ge, Not)

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

  let const_float_block a : Expr.t =
    FloatBlock.make (List.map (fun f -> Expr.F64 f) a)

  let const_float_array a : Expr.t =
    let fields = List.map (fun f -> Expr.F64 f) a in
    Array_new_fixed { typ = FloatArray; fields }

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
            if env.constant_function then
              let closure_id = Closure_id.wrap var in
              WSymbol.get (Compilenv.closure_symbol closure_id)
            else
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

  let bool_not e : Expr.t = Binop (Expr.i32_sub, (I32 1l, e))

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
        else WSymbol.const s
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

  let c_import_type_var (descr : Primitive.description) :
      Type.Var.c_import_func_type =
    let repr_type (t : Primitive.native_repr) : Type.Var.C_import_atom.t =
      if descr.prim_native_name = "" then
        assert (t = Primitive.Same_as_ocaml_repr);
      match t with
      | Same_as_ocaml_repr -> Val
      | Unboxed_float -> Float
      | Unboxed_integer Pnativeint -> I32
      | Unboxed_integer Pint32 -> I32
      | Unboxed_integer Pint64 -> I64
      | Untagged_int -> I32
    in
    let params = List.map repr_type descr.prim_native_repr_args in
    let results = [ repr_type descr.prim_native_repr_res ] in
    { params; results }

  let c_import_type (descr : Type.Var.c_import_func_type) =
    let repr_type (t : Type.Var.C_import_atom.t) : Type.atom =
      match t with
      | Val -> ref_eq
      | Float -> Type.F64
      | I32 -> Type.I32
      | I64 -> Type.I64
    in
    let params = List.map repr_type descr.params in
    let results = List.map repr_type descr.results in
    Decl.Type (C_import_func descr, Func { params; results })

  let conv_apply ~tail env (apply : Flambda.apply) : Expr.t =
    let arity = List.length apply.args in
    State.add_arity arity;
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
          ; body = Call_ref { typ = func_typ; func; args; tail }
          }
      | _ :: _ :: _ ->
        let args =
          Closure.cast (conv_var env apply.func)
          :: List.map (conv_var env) apply.args
        in
        State.add_caml_apply arity;
        let typ = Type.Var.Caml_apply_func { arity } in
        Call { typ; func = Caml_apply arity; args; tail }
    end
    | Direct closure_id ->
      let func = Func_id.of_closure_id closure_id in
      let () =
        let cu = Closure_id.get_compilation_unit closure_id in
        if not (Compilation_unit.is_current cu) then
          State.add_func_import
            { id = closure_id; arity = List.length apply.args }
      in
      let args =
        List.map (conv_var env) apply.args
        @ [ Closure.cast (conv_var env apply.func) ]
      in
      let typ = Type.Var.Func { arity } in
      Call { typ; func; args; tail }

  let conv_allocated_const_expr (const : Allocated_const.t) : Type.atom * Expr.t
      =
    match const with
    | Float f -> (Rvar Float, const_float f)
    | Int32 i -> (Rvar Int32, const_int32 i)
    | Int64 i -> (Rvar Int64, const_int64 i)
    | Nativeint i -> (Rvar Nativeint, const_nativeint i)
    | Immutable_string s | String s -> (Rvar String, const_string s)
    | Immutable_float_array a ->
      (Rvar (BlockFloat { size = List.length a }), const_float_block a)
    | Float_array a ->
      (* TODO/WARNING: this is partially incorrect. Both float
         arrays and mutable blocks containing only floats ends up in
         that case.
         We could either fix the frontend to actually distinguish those cases,
         or fix here and compile BlockFloat as FloatArray *)
      (Rvar FloatArray, const_float_block a)

  let conv_allocated_const (const : Allocated_const.t) : Const.t =
    let typ, e = conv_allocated_const_expr const in
    Expr { typ; e }

  let closure_type ~constant (set_of_closures : Flambda.set_of_closures) =
    let Flambda.{ function_decls; free_vars } = set_of_closures in
    let is_recursive = Variable.Map.cardinal function_decls.funs > 1 in
    if not is_recursive then None
    else begin
      let func_types =
        Variable.Map.fold
          (fun _id (function_decl : Flambda.function_declaration) acc ->
            let arity = Flambda_utils.function_arity function_decl in
            let fields = if constant then 0 else 1 in
            let typ : Type.atom = Rvar (Closure { arity; fields }) in
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

  let closure_types (program : Flambda.program) =
    let list = ref [] in
    Flambda_iterators.iter_on_set_of_closures_of_program program
      ~f:(fun ~constant set_of_closures ->
        match closure_type ~constant set_of_closures with
        | None -> ()
        | Some t -> list := t :: !list );
    !list

  let runtime_prim ~tail name args : Expr.t =
    let arity = List.length args in
    State.add_runtime_import { name; arity };
    let func : Func_id.t = Runtime name in
    let func_type =
      Type.Var.
        { params = List.init arity (fun _ -> Type.Var.C_import_atom.Val)
        ; results = [ Val ]
        }
    in
    let typ = Type.Var.C_import_func func_type in
    State.add_c_import_func_type func_type;
    Call { typ; func; args; tail }

  let unimplemented args =
    let name = Format.asprintf "unimplemented_%i" (List.length args) in
    runtime_prim ~tail:false name args

  let rec conv_body (env : top_env) (expr : Flambda.program_body) effects :
      Module.t =
    match expr with
    | Let_symbol (symbol, Set_of_closures set, body) ->
      let decl = closed_set_of_closures symbol set in
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
      let effect : Expr.t = conv_expr ~tail:false expr_env expr in
      conv_body env body (drop effect :: effects)
    | End _end_symbol ->
      [ Decl.Func
          { name = Start
          ; descr =
              Decl
                { params = []
                ; type_decl = None
                ; body = No_value (NV_seq (List.rev effects))
                }
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
            let expr = conv_expr ~tail:false expr_env expr in
            fields_to_update := (i, expr) :: !fields_to_update;
            I31 dummy_const
          | Some (field : Flambda.constant_defining_value_block_field) -> (
            match field with
            | Symbol s -> WSymbol.const s
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
    let decl = Decl.Const { name; export = Some symbol; descr } in
    let size = List.length fields in
    State.add_block_size size;
    let effect (field, expr) : Expr.no_value_expression =
      Block.set_field ~cast:true ~field ~block:(WSymbol.get symbol) expr
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
            Block.set_field ~cast:true ~field:field_to_update
              ~block:(WSymbol.get symbol)
              (WSymbol.get field_contents) )
          fields_to_update
      in
      ([ Const { name; export = Some symbol; descr } ], new_effects)
    | Project_closure (_sym, _closure_id) -> ([], [])
    | Set_of_closures set ->
      let decl = closed_set_of_closures symbol set in
      (decl, [])
    | Allocated_const const ->
      let name = Global.of_symbol symbol in
      let descr = conv_allocated_const const in
      ([ Const { name; export = Some symbol; descr } ], [])

  and closed_function_declaration (name : Variable.t)
      (declaration : Flambda.function_declaration) : Decl.const =
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
    let closure_symbol = Compilenv.closure_symbol (Closure_id.wrap name) in
    let closure_name = Global.of_symbol closure_symbol in
    { name = closure_name; export = Some closure_symbol; descr = closure }

  and closed_set_of_closures symbol (set_of_closures : Flambda.set_of_closures)
      : Decl.t list =
    let function_decls = set_of_closures.function_decls in
    let is_recursive = Variable.Map.cardinal function_decls.funs > 1 in
    if not is_recursive then begin
      let name, declaration = Variable.Map.choose function_decls.funs in
      let decl = closed_function_declaration name declaration in
      [ Decl.Const decl
      ; Decl.Const
          { name = Global.of_symbol symbol
          ; export = Some symbol (* This export is probably not required *)
          ; descr = Expr { typ = ref_eq; e = Expr.Global_get decl.name }
          }
      ]
    end
    else begin
      let decls =
        Variable.Map.fold
          (fun name (declaration : Flambda.function_declaration) declarations ->
            let decl = closed_function_declaration name declaration in
            decl :: declarations )
          set_of_closures.function_decls.funs []
      in
      let closure_decls = List.map (fun decl -> Decl.Const decl) decls in
      let set_of_closures =
        Const.Struct
          { typ = Set_of_closures function_decls.set_of_closures_id
          ; fields =
              List.rev_map
                (fun (decl : Decl.const) -> Const.Global decl.name)
                decls
          }
      in
      closure_decls
      @ [ Decl.Const
            { name = Global.of_symbol symbol
            ; export =
                Some symbol
                (* This export might not be required, there is no reason for
                   a cross module reference to a set of closure *)
            ; descr = set_of_closures
            }
        ]
    end

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
        State.add_closure_type ~arity ~fields:1;
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

  and conv_function_declaration ~top_env ~closure_functions ~constant_function
      function_name (declaration : Flambda.function_declaration) : Decl.t =
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
        ~constant_function
    in
    let body = conv_expr ~tail:true env declaration.body in
    let func =
      Func.Decl
        { params = params @ [ (Param.Env, Type.Rvar Type.Var.Env) ]
        ; body = Value (body, ref_eq)
        ; type_decl = Some (Func { arity })
        }
    in
    let name = Func_id.of_var_closure_id function_name in
    Decl.Func { name; descr = func }

  and conv_expr ~tail (env : env) (expr : Flambda.t) : Expr.t =
    match expr with
    | Let { var; defining_expr; body = Var v; _ } when Variable.equal var v ->
      conv_named ~tail env defining_expr
    | Let { var; defining_expr; body; _ } ->
      let local = Expr.Local.var_of_var var in
      let defining_expr = conv_named ~tail:false env defining_expr in
      let body = conv_expr ~tail (bind_var env var) body in
      Let { var = local; typ = ref_eq; defining_expr; body }
    | Var var -> conv_var env var
    | Apply apply -> conv_apply ~tail env apply
    | Let_mutable { var; initial_value; contents_kind = _; body } ->
      let local = Expr.Local.var_of_mut_var var in
      let defining_expr = conv_var env initial_value in
      let body = conv_expr ~tail (bind_mutable_var env var) body in
      Let { var = local; typ = ref_eq; defining_expr; body }
    | Assign { being_assigned; new_value } ->
      assert (Mutable_variable.Set.mem being_assigned env.mutables);
      let being_assigned = Expr.Local.var_of_mut_var being_assigned in
      let new_value = conv_var env new_value in
      Unit (Assign { being_assigned; new_value })
    | If_then_else (var, if_expr, else_expr) ->
      let cond : Expr.t = WInt.untag (conv_var env var) in

      let if_expr = conv_expr ~tail env if_expr in
      let else_expr = conv_expr ~tail env else_expr in
      If_then_else { cond; if_expr; else_expr }
    | Static_catch (id, params, body, handler) ->
      let body_env, cont = bind_catch env id in
      let handler_env = List.fold_left bind_var env params in
      let params =
        List.map (fun v -> (Some (Expr.Local.var_of_var v), ref_eq)) params
      in
      let handler = conv_expr ~tail handler_env handler in
      let body = conv_expr ~tail body_env body in
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
      let cond : Expr.t = WInt.untag (conv_expr ~tail:false env cond) in
      let cont = Block_id.fresh "continue" in
      let body : Expr.no_value_expression =
        NV_seq
          [ drop (conv_expr ~tail:false env body)
          ; NV_br_if { cond; if_true = cont }
          ]
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
          match direction with Upto -> Expr.i32_add | Downto -> Expr.i32_sub
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
          [ drop (conv_expr ~tail:false env body)
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
      conv_switch ~tail env cond switch
    | String_switch (cond, branches, default) ->
      let local = Local.fresh "str" in
      let cond = conv_var env cond in
      let body : Expr.t =
        match default with
        | None -> NR Unreachable
        | Some default -> conv_expr ~tail env default
      in
      let body =
        List.fold_left
          (fun body (str, branch) : Expr.t ->
            let cond =
              WInt.untag
                (runtime_prim ~tail:false "string_eq"
                   [ Expr.Var (V local); const_string str ] )
            in
            If_then_else
              { cond; if_expr = conv_expr ~tail env branch; else_expr = body }
            )
          body branches
      in
      Let
        { var = local
        ; typ = Rvar String
        ; defining_expr = Ref_cast { typ = String; r = cond }
        ; body
        }
    | Try_with (body, var, handler) ->
      let local = Expr.Local.var_of_var var in
      (* This is tail in the meaning of wasm: the return value of
         that expression is the return value of the current function.
         It will use stack space, but it is ok to use the call_return *)
      let handler = conv_expr ~tail (bind_var env var) handler in
      Try
        { body = conv_expr ~tail env body
        ; param = (local, ref_eq)
        ; handler
        ; result_typ = ref_eq
        }
    | Proved_unreachable -> NR Unreachable
    | Let_rec (_bindings, _body) ->
      let msg = Format.asprintf "Value letrec not implemented (yet ?)" in
      if ignore_unimplemented then begin
        Format.eprintf "%s@." msg;
        unimplemented []
      end
      else failwith msg
    | Send _ ->
      let msg = Format.asprintf "SEND: objects not implemented (yet ?)" in
      if ignore_unimplemented then begin
        Format.eprintf "%s@." msg;
        unimplemented []
      end
      else failwith msg

  and conv_switch ~tail (env : env) (cond : Expr.t) (switch : Flambda.switch) :
      Expr.t =
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
        if Numbers.Int.Map.is_empty cases then (-1, default_id)
        else
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
          ; handler =
              NR_br { cont = fallthrough; arg = conv_expr ~tail env branch }
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
        (br_table (Block.get_tag ~cast:true) block_cases block_defs)
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
          br_table (Block.get_tag ~cast:true) block_cases block_defs
        in
        NR_if_then_else
          { cond =
              (* TODO refactor to change this into a br_on_cast to avoid casting on i31 later *)
              Unop (Is_i31, cond)
          ; if_expr
          ; else_expr
          }
      in
      make_let_conts body defs

  and conv_named ~tail (env : env) (named : Flambda.named) : Expr.t =
    match named with
    | Prim (prim, args, _dbg) -> conv_prim ~tail env ~prim ~args
    | Symbol s -> WSymbol.get s
    | Expr (Var var) -> conv_var env var
    | Const c ->
      let c = match c with Int i -> i | Char c -> Char.code c in
      Unop (I31_new, I32 (Int32.of_int c))
    | Expr e -> conv_expr ~tail env e
    | Read_symbol_field (symbol, field) ->
      Block.get_field ~field ~cast:true (WSymbol.get symbol)
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

  and conv_prim ~tail env ~(prim : Clambda_primitives.primitive) ~args : Expr.t
      =
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
    let args3 args =
      match args with
      | [ a; b; c ] -> (a, b, c)
      | _ -> Misc.fatal_errorf "Wrong number of primitive arguments"
    in
    let runtime_prim name : Expr.t = runtime_prim ~tail name args in
    let i32 v = WInt.untag v in
    let i31 v = WInt.tag v in
    match prim with
    | Paddint -> i31 (Expr.Binop (Expr.i32_add, args2 (List.map i32 args)))
    | Psubint -> i31 (Expr.Binop (Expr.i32_sub, args2 (List.map i32 args)))
    | Pmulint -> i31 (Expr.Binop (Expr.i32_mul, args2 (List.map i32 args)))
    | Pdivint Unsafe ->
      i31 (Expr.Binop (I_binop (Div S, S32), args2 (List.map i32 args)))
    | Pmodint Unsafe ->
      i31 (Expr.Binop (I_binop (Rem S, S32), args2 (List.map i32 args)))
    | Pnegint ->
      i31 (Binop (Expr.i32_xor, (i32 (arg1 args), I32 (Int32.neg 0l))))
    | Pandint -> i31 (Binop (Expr.i32_and, args2 (List.map i32 args)))
    | Porint -> i31 (Binop (Expr.i32_or, args2 (List.map i32 args)))
    | Pxorint -> i31 (Binop (Expr.i32_xor, args2 (List.map i32 args)))
    | Plslint -> i31 (Binop (Expr.i32_shl, args2 (List.map i32 args)))
    | Plsrint -> i31 (Binop (Expr.i32_shr_u, args2 (List.map i32 args)))
    | Pasrint -> i31 (Binop (Expr.i32_shr_s, args2 (List.map i32 args)))
    | Poffsetint n ->
      i31 (Expr.Binop (Expr.i32_add, (i32 (arg1 args), I32 (Int32.of_int n))))
    | Poffsetref n ->
      let ref_val = Block.get_field ~cast:true (arg1 args) ~field:0 in
      let value =
        i31 (Expr.Binop (Expr.i32_add, (i32 ref_val, I32 (Int32.of_int n))))
      in
      Unit (Block.set_field ~cast:true ~block:(arg1 args) ~field:0 value)
    | Pisout -> i31 (I_relop (S32, Lt U, args2 (List.map i32 args)))
    | Pabsfloat -> box_float (Unop (Abs_float, unbox_float (arg1 args)))
    | Pnegfloat -> box_float (Unop (Neg_float, unbox_float (arg1 args)))
    | Paddfloat ->
      box_float (Expr.Binop (Expr.f64_add, args2 (List.map unbox_float args)))
    | Psubfloat ->
      box_float (Expr.Binop (Expr.f64_sub, args2 (List.map unbox_float args)))
    | Pmulfloat ->
      box_float (Expr.Binop (Expr.f64_mul, args2 (List.map unbox_float args)))
    | Pdivfloat ->
      box_float (Expr.Binop (Expr.f64_div, args2 (List.map unbox_float args)))
    | Pfloatcomp cmp -> begin
      let relop, is_not = float_comparision cmp in
      let cmp_op : Expr.t =
        F_relop (S64, relop, args2 (List.map unbox_float args))
      in
      let op = match is_not with Id -> cmp_op | Not -> bool_not cmp_op in
      i31 op
    end
    | Pintoffloat ->
      i31
        (Unop
           ( Trunc { from_type = S64; to_type = S32; sign = S }
           , unbox_float (arg1 args) ) )
    | Pfloatofint ->
      box_float
        (Unop
           ( Convert { from_type = S32; to_type = S64; sign = S }
           , i32 (arg1 args) ) )
    | Pccall descr ->
      let unbox_arg (t : Primitive.native_repr) arg =
        match t with
        | Same_as_ocaml_repr -> arg
        | Unboxed_float -> unbox_float arg
        | Unboxed_integer kind -> WBint.unbox kind arg
        | Untagged_int -> i32 arg
      in
      let box_result (t : Primitive.native_repr) res =
        match t with
        | Same_as_ocaml_repr -> res
        | Unboxed_float -> box_float res
        | Unboxed_integer kind -> WBint.box kind res
        | Untagged_int -> i31 res
      in
      let func_typ = c_import_type_var descr in
      let typ = Type.Var.C_import_func func_typ in
      State.add_c_import descr;
      State.add_c_import_func_type func_typ;
      let args = List.map2 unbox_arg descr.prim_native_repr_args args in
      let tail =
        match descr.prim_native_repr_res with
        | Same_as_ocaml_repr -> tail
        | _ -> false
      in
      box_result descr.prim_native_repr_res
        (Call { typ; tail; args; func = Func_id.prim_name descr })
    | Pmakeblock (tag, _mut, _shape) ->
      let size = List.length args in
      State.add_block_size size;
      Struct_new
        ( Block { size }
        , I32 (Int32.of_int tag) :: I32 (Int32.of_int size) :: args )
    | Pfield field ->
      let arg = arg1 args in
      Block.get_field ~field ~cast:true arg
    | Psetfield (field, _kind, _init) ->
      let block, value = args2 args in
      Seq ([ Block.set_field ~cast:true ~field ~block value ], unit_value)
    | Pmakearray (Pfloatarray, Immutable) ->
      FloatBlock.make (List.map unbox_float args)
    | Pmakearray (Pfloatarray, Mutable) ->
      Array_new_fixed { typ = FloatArray; fields = List.map unbox_float args }
    | Pmakearray ((Pintarray | Paddrarray), _) ->
      Array_new_fixed { typ = Array; fields = args }
    | Pmakearray (Pgenarray, _) -> begin
      match args with
      | [] -> Array_new_fixed { typ = Array; fields = [] }
      | first :: _ ->
        let cont = Block_id.fresh "make_float_array" in
        let handler : Expr.t =
          Array_new_fixed
            { typ = FloatArray; fields = List.map unbox_float args }
        in
        let if_else : Expr.t = Array_new_fixed { typ = Array; fields = args } in
        Let_cont
          { cont
          ; params = [ (None, Rvar Float) ]
          ; handler
          ; body =
              Br_on_cast { value = first; typ = Float; if_cast = cont; if_else }
          }
    end
    | Pduparray (_, _) -> runtime_prim "duparray"
    | Pfloatfield field ->
      let arg = arg1 args in
      box_float (FloatBlock.get_field ~field arg)
    | Psetfloatfield (field, _init) ->
      let block, value = args2 args in
      Seq
        ([ FloatBlock.set_field ~field ~block (unbox_float value) ], unit_value)
    | Popaque -> arg1 args
    | Pisint -> WInt.tag (Unop (Is_i31, arg1 args))
    | Pintcomp Ceq -> i31 (Expr.Binop (Ref_eq, args2 args))
    | Pintcomp cop -> begin
      let op : Expr.t =
        match cop with
        | Ceq -> Binop (Ref_eq, args2 args)
        | Cne -> bool_not (Binop (Ref_eq, args2 args))
        | Clt | Cgt | Cle | Cge ->
          let cmp = integer_comparision cop in
          I_relop (S32, cmp, args2 (List.map i32 args))
      in
      i31 op
    end
    | Pcompare_ints -> runtime_prim "compare_ints"
    | Pcompare_floats -> runtime_prim "compare_floats"
    | Pcompare_bints Pint64 -> runtime_prim "compare_i64"
    | Pcompare_bints Pint32 -> runtime_prim "compare_i32"
    | Pcompare_bints Pnativeint -> runtime_prim "compare_nativeint"
    | Pbintofint t ->
      WBint.box t (WBint.conv ~from:Pint32 ~to_:t (i32 (arg1 args)))
    | Pintofbint t ->
      i31 (WBint.conv ~from:t ~to_:Pint32 (WBint.unbox t (arg1 args)))
    | Pcvtbint (from, to_) ->
      WBint.box to_ (WBint.conv ~from ~to_ (WBint.unbox from (arg1 args)))
    | Pnegbint t ->
      let args = (WBint.unbox t (arg1 args), WBint.unboxed_const t 0) in
      WBint.box t (Binop (I_binop (Xor, WBint.size t), args))
    | Paddbint t -> WBint.binop t Add (args2 args)
    | Psubbint t -> WBint.binop t Sub (args2 args)
    | Pmulbint t -> WBint.binop t Mul (args2 args)
    | Pandbint t -> WBint.binop t And (args2 args)
    | Porbint t -> WBint.binop t Or (args2 args)
    | Pxorbint t -> WBint.binop t Xor (args2 args)
    | Pdivbint { size = t; is_safe = Unsafe } ->
      WBint.binop t (Div S) (args2 args)
    | Pmodbint { size = t; is_safe = Unsafe } ->
      WBint.binop t (Rem S) (args2 args)
    | Plslbint t -> WBint.binop_with_int t Shl (args2 args)
    | Plsrbint t -> WBint.binop_with_int t (Shr U) (args2 args)
    | Pasrbint t -> WBint.binop_with_int t (Shr S) (args2 args)
    | Pbintcomp (t, cop) -> WBint.relop t cop (args2 args)
    | Praise _raise_kind -> Throw (arg1 args)
    | Pbyteslength | Pstringlength ->
      let typ : Type.Var.t = String in
      i31 (Unop (Array_len typ, Ref_cast { typ; r = arg1 args }))
    | Parraylength (Paddrarray | Pintarray) ->
      let typ : Type.Var.t = Array in
      i31 (Unop (Array_len typ, Ref_cast { typ; r = arg1 args }))
    | Parraylength Pfloatarray ->
      let typ : Type.Var.t = FloatArray in
      i31 (Unop (Array_len typ, Ref_cast { typ; r = arg1 args }))
    | Parraylength Pgenarray -> runtime_prim "array_length"
    | Parrayrefu (Paddrarray | Pintarray) ->
      let typ : Type.Var.t = Array in
      let array, field = args2 args in
      Binop (Array_get typ, (Ref_cast { typ; r = array }, i32 field))
    | Parrayrefu Pfloatarray ->
      let typ : Type.Var.t = FloatArray in
      let array, field = args2 args in
      box_float (Binop (Array_get typ, (Ref_cast { typ; r = array }, i32 field)))
    | Parrayrefu Pgenarray -> runtime_prim "array_get_unsafe"
    | Parrayrefs (Paddrarray | Pintarray) ->
      runtime_prim "array_get_int_or_addr_safe"
    | Parrayrefs Pfloatarray -> runtime_prim "array_get_float_safe"
    | Parrayrefs Pgenarray -> runtime_prim "array_get_safe"
    | Parraysetu (Paddrarray | Pintarray) ->
      let typ : Type.Var.t = Array in
      let array, field, value = args3 args in
      Unit
        (Array_set
           { typ
           ; array = Ref_cast { typ; r = array }
           ; field = i32 field
           ; value
           } )
    | Parraysetu Pfloatarray ->
      let typ : Type.Var.t = FloatArray in
      let array, field, value = args3 args in
      Unit
        (Array_set
           { typ
           ; array = Ref_cast { typ; r = array }
           ; field = i32 field
           ; value = unbox_float value
           } )
    | Parraysetu Pgenarray -> runtime_prim "array_set_unsafe"
    | Parraysets (Paddrarray | Pintarray) ->
      runtime_prim "array_set_int_or_addr_safe"
    | Parraysets Pfloatarray -> runtime_prim "array_set_float_safe"
    | Parraysets Pgenarray -> runtime_prim "array_set_safe"
    | Pnot -> i31 (bool_not (i32 (arg1 args)))
    | Pbytesrefs | Pstringrefs -> runtime_prim "string_get"
    | Pbytesrefu | Pstringrefu ->
      let arr, idx = args2 args in
      i31
        (Binop
           ( Array_get_packed { typ = String; extend = S }
           , (Ref_cast { typ = String; r = arr }, i32 idx) ) )
    | Pbytessets -> runtime_prim "bytes_set"
    | Pbytessetu ->
      let array, field, value = args3 args in
      Unit
        (Array_set
           { typ = String
           ; array = Ref_cast { typ = String; r = array }
           ; field = i32 field
           ; value = i32 value
           } )
    | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _ | Pbigstring_load _
    | Pbigstring_set _ ->
      unimplemented args
    | Pstring_load _ | Pbytes_load _ | Pbytes_set _ | Pbswap16 | Pbbswap _
    | Pint_as_pointer ->
      unimplemented args
    | _ ->
      let msg =
        Format.asprintf "TODO prim %a" Printclambda_primitives.primitive prim
      in
      failwith msg

  let conv_functions ~top_env (flambda : Flambda.program) =
    let constant_sets =
      Flambda_utils.all_lifted_constant_sets_of_closures flambda
    in
    List.fold_left
      (fun decls (set_of_closures : Flambda.set_of_closures) ->
        let function_decls = set_of_closures.function_decls in
        let constant_function =
          Set_of_closures_id.Set.mem function_decls.set_of_closures_id
            constant_sets
        in
        let closure_functions = Variable.Map.keys function_decls.funs in
        Variable.Map.fold
          (fun var function_declaration decls ->
            let decl =
              conv_function_declaration ~top_env ~closure_functions var
                ~constant_function function_declaration
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
    Func { params = params @ [ Type.Rvar Env ]; results = [ ref_eq ] }

  let caml_apply_type size : Type.descr =
    let params = List.init size (fun _ -> ref_eq) in
    Func { params = [ Type.Rvar Env ] @ params; results = [ ref_eq ] }

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
         (Expr.Call_ref
            { tail = true; typ = Type.Var.Func { arity = n }; args; func } ) )

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
      ; type_decl = Some (Func { arity = 1 })
      }

  let caml_apply n =
    (* TODO apply direct if right number of arguments *)
    let closure_param = Param.P ("closure", 0) in
    let param_i i = Param.P ("param", i) in
    let params = List.init n (fun i -> (param_i i, ref_eq)) in
    let rec build closure_var n params : Expr.t =
      let mk_call ~tail param =
        Expr.Call_ref
          { typ = Func { arity = 1 }
          ; args = [ Var (Param param); Var closure_var ]
          ; func = Closure.get_gen_func (Var closure_var)
          ; tail
          }
      in
      match params with
      | [] -> assert false
      | [ (param, _typ) ] -> mk_call ~tail:true param
      | (param, _typ) :: params ->
        let var : Expr.Local.var = Fresh ("partial_closure", n) in
        let call : Expr.t = Closure.cast (mk_call ~tail:false param) in
        let body = build (Expr.Local.V var) (n + 1) params in
        Let { var; typ = Rvar Env; defining_expr = call; body }
    in
    let body = build (Param closure_param) 0 params in
    Func.Decl
      { params = (closure_param, Type.Rvar Env) :: params
      ; body = Value (body, ref_eq)
      ; type_decl = None
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

  let global_import (sym : Global_import.t) =
    let module_ =
      if Compilenv.is_predefined_exception sym then "runtime"
      else
        Linkage_name.to_string
        @@ Compilation_unit.get_linkage_name (Symbol.compilation_unit sym)
    in
    let name = WSymbol.export_name sym in
    Const.Import { typ = ref_eq; module_; name }

  let func_import ({ id; arity } : Func_import.t) =
    let module_ =
      Linkage_name.to_string
      @@ Compilation_unit.get_linkage_name (Closure_id.get_compilation_unit id)
    in
    let name =
      let name, n = Variable.unique_name_id (Closure_id.unwrap id) in
      Format.asprintf "%s_%i" name n
      (* Func_id.name (Func_id.of_closure_id id) *)
    in
    let params = List.init arity (fun _ -> ref_eq) @ [ Type.Rvar Env ] in
    Func.Import { params; result = [ ref_eq ]; module_; name }

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
      Global_import.Set.fold
        (fun (sym : Global_import.t) decls ->
          let name = Global.Sym sym in
          let descr = global_import sym in
          Decl.Const { name; export = None; descr } :: decls )
        !State.global_imports decls
    in
    let decls =
      Func_import.Set.fold
        (fun (import : Func_import.t) decls ->
          let name = Func_id.of_closure_id import.id in
          let descr = func_import import in
          Decl.Func { name; descr } :: decls )
        !State.func_imports decls
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
    let decls =
      C_import_func_type.Set.fold
        (fun (descr : Type.Var.c_import_func_type) decls ->
          c_import_type descr :: decls )
        !State.c_import_func_types decls
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
      Arity.Set.fold
        (fun arity decls ->
          let name = Type.Var.Caml_apply_func { arity } in
          let descr = caml_apply_type arity in
          Decl.Type (name, descr) :: decls )
        (Arity.Set.remove 1 !State.caml_applies)
        decls
    in
    let decls =
      float_type :: int32_type :: int64_type :: nativeint_type :: string_type
      :: array_type :: floatarray_type :: func_1_and_env :: decls
    in
    decls
end

module ToWasm = struct
  module Cst = Wast.Cst
  module C = Wast.C

  let option_to_list = function None -> [] | Some v -> [ v ]

  let tvar v = Type.Var.name v

  let gvar v = Global.name v

  let unit = Cst.node "i31.new" [ C.i32 0l ]

  let nn_name (nn : Expr.nn) = match nn with S32 -> "32" | S64 -> "64"

  let sx_name (sx : Expr.sx) = match sx with S -> "s" | U -> "u"

  let ibinop_name (op : Expr.ibinop) =
    match op with
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div s -> Printf.sprintf "div_%s" (sx_name s)
    | Rem s -> Printf.sprintf "rem_%s" (sx_name s)
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"
    | Shl -> "shl"
    | Shr s -> Printf.sprintf "shr_%s" (sx_name s)
    | Rotl -> "rotl"
    | Rotr -> "rotr"

  let fbinop_name (op : Expr.fbinop) =
    match op with
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "div"
    | Min -> "min"
    | Max -> "max"
    | Copysign -> "copysign"

  let conv_binop (op : Expr.binop) args =
    match op with
    | I_binop (op, size) ->
      let name = Printf.sprintf "i%s.%s" (nn_name size) (ibinop_name op) in
      Cst.node name args
    | F_binop (op, size) ->
      let name = Printf.sprintf "f%s.%s" (nn_name size) (fbinop_name op) in
      Cst.node name args
    | Ref_eq -> Cst.node "ref.eq" args
    | Array_get typ -> C.array_get typ args
    | Array_get_packed { typ; extend } -> C.array_get_packed typ extend args

  let conv_nv_binop (op : Expr.nv_binop) block value =
    match op with
    | Struct_set { typ; field } -> [ C.struct_set typ field block value ]

  let num_type_name (n : Expr.num_type) =
    match n with
    | I S32 -> "i32"
    | I S64 -> "i64"
    | F S32 -> "f32"
    | F S64 -> "f64"

  let size_name (nn : Expr.nn) = match nn with S64 -> "64" | S32 -> "32"

  let sign_name (sx : Expr.sx) = match sx with S -> "s" | U -> "u"

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
    | Is_i31 -> begin
      match mode with
      | Binarien -> Cst.node "ref.is_i31" [ arg ]
      | Reference -> Cst.node "ref.test" [ Cst.atom "i31"; arg ]
    end
    | Array_len t -> C.array_len t arg
    | Reinterpret { from_type; to_type } ->
      let name =
        Printf.sprintf "%s.reinterpret_%s" (num_type_name to_type)
          (num_type_name from_type)
      in
      Cst.node name [ arg ]
    | I32_wrap_i64 -> Cst.node "i32.wrap_i64" [ arg ]
    | I64_extend_i32 sign ->
      Cst.node (Printf.sprintf "i64.extend_%s_i32" (sign_name sign)) [ arg ]
    | Convert { from_type; to_type; sign } ->
      let name =
        Printf.sprintf "f%s.convert_i%s_%s" (size_name to_type)
          (size_name from_type) (sign_name sign)
      in
      Cst.node name [ arg ]
    | Trunc { from_type; to_type; sign } ->
      let name =
        Printf.sprintf "i%s.trunc_f%s_%s" (size_name to_type)
          (size_name from_type) (sign_name sign)
      in
      Cst.node name [ arg ]
    | Abs_float -> Cst.node "f64.abs" [ arg ]
    | Neg_float -> Cst.node "f64.neg" [ arg ]

  let irelop_name nn (op : Expr.irelop) =
    match op with
    | Eq -> Format.asprintf "i%s.eq" (nn_name nn)
    | Ne -> Format.asprintf "i%s.ne" (nn_name nn)
    | Lt sx -> Format.asprintf "i%s.lt_%s" (nn_name nn) (sx_name sx)
    | Gt sx -> Format.asprintf "i%s.gt_%s" (nn_name nn) (sx_name sx)
    | Le sx -> Format.asprintf "i%s.le_%s" (nn_name nn) (sx_name sx)
    | Ge sx -> Format.asprintf "i%s.ge_%s" (nn_name nn) (sx_name sx)

  let conv_irelop nn op a1 a2 = Cst.node (irelop_name nn op) [ a1; a2 ]

  let frelop_name nn (op : Expr.frelop) =
    match op with
    | Eq -> Format.asprintf "f%s.eq" (nn_name nn)
    | Ne -> Format.asprintf "f%s.ne" (nn_name nn)
    | Lt -> Format.asprintf "f%s.lt" (nn_name nn)
    | Gt -> Format.asprintf "f%s.gt" (nn_name nn)
    | Le -> Format.asprintf "f%s.le" (nn_name nn)
    | Ge -> Format.asprintf "f%s.ge" (nn_name nn)

  let conv_frelop nn op a1 a2 = Cst.node (frelop_name nn op) [ a1; a2 ]

  let group e = match e with [ v ] -> v | _ -> C.group_block [ ref_eq ] e

  let rec conv_expr (expr : Expr.t) : Cst.t list =
    match expr with
    | Var v -> [ C.local_get v ]
    | Binop (op, (arg1, arg2)) ->
      [ conv_binop op [ conv_expr_group arg1; conv_expr_group arg2 ] ]
    | I_relop (nn, op, (arg1, arg2)) ->
      [ conv_irelop nn op (conv_expr_group arg1) (conv_expr_group arg2) ]
    | F_relop (nn, op, (arg1, arg2)) ->
      [ conv_frelop nn op (conv_expr_group arg1) (conv_expr_group arg2) ]
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
    | Call_ref { typ; args; func; tail } ->
      let args = List.map conv_expr_group args @ [ conv_expr_group func ] in
      if tail then [ C.return_call_ref typ args ] else [ C.call_ref typ args ]
    | Call { typ; args; func; tail } ->
      let args = List.map conv_expr_group args in
      if tail then
        (* This should be
           {[ [ C.return_call func args ] ]}
           But return call is not handled by the gc branch so we play a trick
           with return_call_ref
        *)
        [ C.return_call_ref typ (args @ [ C.ref_func func ]) ]
      else [ C.call func args ]
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
      [ C.drop (C.br_on_cast if_cast typ (conv_expr_group value)) ]
      @ conv_expr if_else
    | Br_if { cond; if_true; if_else } ->
      [ C.br_if if_true (conv_expr_group cond) ] @ conv_expr if_else
    | Br_table { cond; cases; default } ->
      [ C.br_table (conv_expr_group cond) (cases @ [ default ]) ]
    | Throw e -> begin
      match mode with
      | Reference -> [ C.unreachable ]
      | Binarien -> [ C.throw (conv_expr_group e) ]
    end
    | Try { body; handler; result_typ; param = local, typ } -> begin
      match mode with
      | Reference ->
        Format.eprintf "Warning exception not supported@.";
        conv_expr body
      | Binarien ->
        let body = conv_expr body in
        let handler = C.local_set (V local) (C.pop typ) :: conv_expr handler in
        [ C.try_ ~result_typ ~body ~handler ~typ ]
    end
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
    | Array_set { typ; array; field; value } ->
      [ C.array_set typ
          [ conv_expr_group array
          ; conv_expr_group field
          ; conv_expr_group value
          ]
      ]
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
    | Unreachable -> [ C.unreachable ]

  let conv_const name export (const : Const.t) =
    let export_name = Option.map Conv.WSymbol.export_name export in
    match const with
    | Struct { typ; fields } ->
      let field (field : Const.field) : Cst.t =
        match field with
        | I8 i | I16 i -> C.i32_ i
        | I31 i -> C.i31_new (C.i32_ i)
        | Ref_func f -> C.ref_func f
        | Global g -> C.global_get g
      in
      C.global (Global.name name) export_name (C.reft typ)
        [ C.struct_new_canon typ (List.map field fields) ]
    | Expr { typ; e } ->
      C.global (Global.name name) export_name (C.type_atom typ) (conv_expr e)
    | Import { typ; module_; name = import_name } ->
      C.global_import (Global.name name) (C.type_atom typ) module_ import_name

  let conv_func name (func : Func.t) =
    match func with
    | Import { module_; name = prim_name; params; result } ->
      let typ = C.func_type ~name params result in
      [ (* The declare shouldn't be required: the should not be any reference to an
           imported function, but calls are compiled as ref_call for the tail rec
           hack so we need that for now *)
        C.declare_func name
      ; C.import module_ prim_name typ
      ]
    | Decl { params; body; type_decl } ->
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
        C.func ~name ~type_decl ~params ~locals ~result ~body
      in
      [ C.declare_func name; func ]

  let conv_type name (descr : Type.descr) =
    match descr with
    | Struct { sub; fields } ->
      let descr = C.struct_type ~sub fields in
      C.type_ name descr
    | Array { sub; fields } ->
      let descr = C.array_type fields in
      let descr =
        match sub with None -> descr | Some sub -> C.sub sub descr
      in
      C.type_ name descr
    | Func { params; results } -> C.type_ name (C.func_type params results)

  let conv_type_rec types =
    C.rec_ (List.map (fun (name, descr) -> conv_type name descr) types)

  let rec conv_decl = function
    | [] -> [ C.start Start ]
    | Decl.Const { name; export; descr } :: tl ->
      conv_const name export descr :: conv_decl tl
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

let output_file ~output_prefix ~module_ ~register =
  let wastfile = output_prefix ^ ".wast" in
  let oc = open_out_bin wastfile in
  let ppf = Format.formatter_of_out_channel oc in
  Misc.try_finally
    ~always:(fun () ->
      Format.fprintf ppf "@.";
      close_out oc )
    (* ~exceptionally:(fun () -> Misc.remove_file wastfile) *)
      (fun () ->
      ToWasm.Cst.emit ppf module_;
      Format.fprintf ppf "@\n";
      ToWasm.Cst.emit ppf register )

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
  let wasm =
    Profile.record_call "ToWasm" (fun () -> ToWasm.conv_module (common @ m))
  in
  let register =
    let ln =
      Compilation_unit.get_linkage_name (Compilation_unit.get_current_exn ())
    in
    Wast.C.register (Linkage_name.to_string ln)
  in
  (* Format.printf "@.%a@." ToWasm.Cst.emit wasm; *)
  Profile.record_call "output_wasm" (fun () ->
      output_file ~output_prefix ~module_:wasm ~register )

let emit ~output_prefix (flambda : Flambda.program) = run ~output_prefix flambda
