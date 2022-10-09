[@@@ocaml.warning "-23"]
[@@@ocaml.warning "-37"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-60"]

let print_list f sep ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "%s@ " sep)
    f ppf l

module Arity = struct
  type t = int
  module Set = Set.Make (Int)
end

module State = struct
  let arities = ref Arity.Set.empty

  let add_arity i = arities := Arity.Set.add i !arities

  let reset () = arities := Arity.Set.empty
end

module Type = struct
  module Var = struct
    type t =
      | V of string * int
      | Partial_closure of int * int
      | Func of { arity : int }
      | Closure of { arity : int; fields : int }
      | Env
      | Block of { size : int }

    let name = function
      | V (name, n) -> Format.asprintf "%s_%i" name n
      | Partial_closure (n, m) -> Format.asprintf "Partial_closure_%i_%i" n m
      | Env -> Format.asprintf "Env"
      | Func { arity } -> Format.asprintf "Func_%i" arity
      | Block { size } -> Format.asprintf "Block_%i" size
      | Closure { arity; fields } -> Format.asprintf "Closure_%i_%i" arity fields

    let print ppf v =
      Format.pp_print_string ppf (name v)

  end

  type atom =
    | I8
    | I16
    | Any
    | Rvar of Var.t

  type descr =
    | Struct of atom list
    | Func of
        { args : atom list;
          result : atom option
        }

  let print_atom ppf = function
    | I8 -> Format.fprintf ppf "i8"
    | I16 -> Format.fprintf ppf "i16"
    | Any -> Format.fprintf ppf "ref_any"
    | Rvar v -> Format.fprintf ppf "ref_%a" Var.print v

  let print_descr ppf = function
    | Struct atoms ->
      Format.fprintf ppf "@[<hov 2>Struct {%a}@]"
        (print_list print_atom ";")
        atoms
    | Func { args; result = None } ->
      Format.fprintf ppf "@[<hov 2>Func {%a}@]" (print_list print_atom ",") args
    | Func { args; result = Some result } ->
      Format.fprintf ppf "@[<hov 2>Func {%a} ->@ %a@]"
        (print_list print_atom ",")
        args print_atom result
end

module Func_id = struct
  type t =
    | V of string * int
    | Symbol of Symbol.t
    | Caml_curry of int * int

  let name = function
    | V (name, n) -> Format.asprintf "%s_%i" name n
    | Symbol s -> Format.asprintf "%a" Symbol.print s
    | Caml_curry (n, m) ->
      if m = 0
      then Format.asprintf "Caml_curry_%i" n
      else Format.asprintf "Caml_curry_%i_%i" n m

  let print ppf = function
    | V (name, n) -> Format.fprintf ppf "%s_%i" name n
    | Symbol s -> Symbol.print ppf s
    | Caml_curry (n, m) ->
      if m = 0
      then Format.fprintf ppf "Caml_curry_%i" n
      else Format.fprintf ppf "Caml_curry_%i_%i" n m
end

module Param = struct
  type t =
    | P of string * int
    | Env
  let print ppf = function
    | P (name, n) -> Format.fprintf ppf "P(%s_%i)" name n
    | Env -> Format.fprintf ppf "Env"

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
    type t =
      | V of var
      | Param of Param.t
    let print ppf = function
      | V v -> print_var ppf v
      | Param p -> Param.print ppf p
    let var_of_var v = Variable.unique_name_id v
    let of_var v = V (var_of_var v)
  end

  type binop =
    | I32_add
    | I32_sub

  type unop =
    | I31_get_s
    | I31_new

  type t =
    | Var of Local.t
    | I32 of int32
    | Ref_func of Func_id.t
    | Let of
        { var : Local.var;
          defining_expr : t;
          body : t
        }
    | Binop of binop * (t * t)
    | Unop of unop * t
    | Struct_new of Type.Var.t * t list
    | Struct_get of
        { typ : Type.Var.t;
          block : t;
          field : t
        }
    | Call_ref of
        { typ : Type.Var.t;
          args : t list;
          func : t
        }
    | Ref_cast of
        { typ : Type.Var.t;
          r : t
        }

  let print_binop ppf = function
    | I32_add -> Format.fprintf ppf "I32_add"
    | I32_sub -> Format.fprintf ppf "I32_sub"

  let print_unop ppf = function
    | I31_get_s -> Format.fprintf ppf "I31_get_s"
    | I31_new -> Format.fprintf ppf "I31_new"

  let rec print ppf = function
    | Var l -> Local.print ppf l
    | I32 i -> Format.fprintf ppf "%li" i
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
    | Struct_get { typ; block; field } ->
      Format.fprintf ppf "@[<hov 2>Struct_get(%a:@ %a).(%a)@]" Type.Var.print
        typ print block print field
    | Call_ref { typ; args; func } ->
      Format.fprintf ppf "@[<hov 2>Call_ref(%a:@ %a(%a))@]" Type.Var.print typ
        print func (print_list print ",") args
    | Ref_cast { typ; r } ->
      Format.fprintf ppf "@[<hov 2>Ref_cast(%a:@ %a)@]" Type.Var.print typ print
        r

  let let_ var defining_expr body = Let { var; defining_expr; body }
end

module Func = struct
  type t =
    | Decl of
        { params : (Param.t * Type.atom) list;
          result : Type.atom option;
          body : Expr.t
        }
    | Import of
        { typ : Type.Var.t;
          module_ : string;
          name : string
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
    | Import { typ; module_; name } ->
      Format.fprintf ppf "@[<hov 2>Import %a %s %s@]" Type.Var.print typ module_
        name
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
        { typ : Type.Var.t;
          fields : field list
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
    (* | Declare_func of Expr.Var.t *)
    | Func of
        { name : Func_id.t;
          descr : Func.t
        }
    | Const of
        { name : Global.t;
          descr : Const.t
        }

  let print ppf = function
    | Type (var, descr) ->
      Format.fprintf ppf "type %a = %a" Type.Var.print var Type.print_descr
        descr
    | Func { name; descr } ->
      Format.fprintf ppf "@[<hov 2>func %a =@ %a@]" Func_id.print name
        Func.print descr
    | Const { name; descr = Struct { typ; fields } } ->
      Format.fprintf ppf "@[<hov 2>const %a : %a =@ {%a}@]" Global.print name
        Type.Var.print typ
        (print_list Const.print_field ";")
        fields
end

module Module = struct
  type t = Decl.t list

  let print ppf l =
    Format.fprintf ppf "@[<v 2>Module {@ %a@ }@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
         Decl.print)
      l
end

module Conv = struct
  type env =
    { bound_vars : Variable.Set.t;
      params : Variable.Set.t;
      closure_vars : Variable.Set.t
    }
  let enter_function ~params ~free_vars =
    let params =
      List.fold_left
        (fun params p -> Variable.Set.add (Parameter.var p) params)
        Variable.Set.empty params
    in
    let closure_vars = Variable.Set.diff free_vars params in
    { bound_vars = Variable.Set.empty; params; closure_vars }
  let bind_var env var =
    { env with bound_vars = Variable.Set.add var env.bound_vars }

  let conv_var (env : env) (var : Variable.t) : Expr.t =
    begin
      if Variable.Set.mem var env.params
      then Var (Expr.Local.Param (Param.of_var var))
      else if Variable.Set.mem var env.bound_vars
      then Var (Expr.Local.of_var var)
      else if Variable.Set.mem var env.closure_vars
      then failwith "TODO"
      else Misc.fatal_errorf "Unbound variable %a" Variable.print var
    end

  let const_block tag fields : Const.t =
    let field (f : Flambda.constant_defining_value_block_field) : Const.field =
      match f with
      | Symbol s -> Global (Global.of_symbol s)
      | Const (Int i) -> I31 i
      | Const (Char c) -> I31 (Char.code c)
    in
    let fields =
      [Const.I8 (Tag.to_int tag); Const.I16 (List.length fields)] @ List.map field fields
    in
    Struct { typ = (Type.Var.Block { size = List.length fields }); fields }

  let rec conv_body (expr : Flambda.program_body) : Module.t =
    match expr with
    | Let_symbol (symbol, Set_of_closures set, body) ->
      let decl = closed_function_declarations symbol set.function_decls in
      let body = conv_body body in
      decl @ body
    | Let_symbol (symbol, Block (tag, fields), body) ->
      let name = Global.of_symbol symbol in
      let descr = const_block tag fields in
      let body = conv_body body in
      Const { name; descr } :: body
    | Let_symbol (_symbol, _const, body) ->
      Format.printf "IGNORE LET SYMBOL@.";
      conv_body body
    | Let_rec_symbol (_, body) ->
      Format.printf "IGNORE LET REC SYMBOL@.";
      conv_body body
    | Initialize_symbol (_, _, _, body) ->
      Format.printf "IGNORE INITIALIZE SYMBOL@.";
      conv_body body
    | Effect (_, body) ->
      Format.printf "IGNORE EFFECT@.";
      conv_body body
    | End _end_symbol -> []

  and closed_function_declarations _symbol
      (declarations : Flambda.function_declarations) : Decl.t list =
    Variable.Map.fold
      (fun name declaration declarations ->
        let function_name = Func_id.V (Variable.name name, 0) in
        let e, closure =
          closed_function_declaration function_name declaration
        in
        let closure_name = Global.Closure name in
        Decl.Func { name = function_name; descr = e }
        :: Decl.Const { name = closure_name; descr = closure }
        :: declarations)
      declarations.funs []

  and closed_function_declaration function_name
      (declaration : Flambda.function_declaration) : Func.t * Const.t =
    let arity = List.length declaration.params in
    let params =
      List.map
        (fun p -> Param.of_var (Parameter.var p), Type.Any)
        declaration.params
    in
    let env =
      enter_function ~params:declaration.params
        ~free_vars:declaration.free_variables
    in
    let body = conv_expr env declaration.body in
    let func =
      Func.Decl
        { params = params @ [Param.Env, Type.Rvar Type.Var.Env];
          result = Some Type.Any;
          body
        }
    in
    let closure =
      let fields : Const.field list =
        State.add_arity arity;
        if arity = 1
        then [I8 1; Ref_func function_name]
        else
          [ I8 arity;
            Ref_func (Func_id.Caml_curry (arity, 0));
            Ref_func function_name ]
      in
      Const.Struct { typ = Type.Var.Closure { arity; fields = 0 }; fields }
    in
    func, closure

  and conv_expr env (expr : Flambda.t) : Expr.t =
    match expr with
    | Let { var; defining_expr; body; _ } ->
      let local = Expr.Local.var_of_var var in
      let defining_expr = conv_named env defining_expr in
      let body = conv_expr (bind_var env var) body in
      Let { var = local; defining_expr; body }
    | Var var -> conv_var env var
    | _ -> failwith "TODO"

  and conv_named env (named : Flambda.named) : Expr.t =
    match named with
    | Prim (prim, args, _dbg) -> conv_prim env ~prim ~args
    | _ -> failwith "TODO"

  and conv_prim env ~(prim : Clambda_primitives.primitive) ~args : Expr.t =
    let args = List.map (conv_var env) args in
    let args2 args =
      match args with
      | [a; b] -> a, b
      | _ -> Misc.fatal_errorf "Wrong number of primitive arguments"
    in
    let i32 v = Expr.Unop (I31_get_s, v) in
    let i31 v = Expr.Unop (I31_new, v) in
    match prim with
    | Paddint -> i31 (Expr.Binop (I32_add, args2 (List.map i32 args)))
    | Psubint -> i31 (Expr.Binop (I32_sub, args2 (List.map i32 args)))
    | _ -> failwith "TODO"

  let caml_curry_apply ~param_arg ~env_arg n =
    let closure_arg_typ = Type.Var.Partial_closure (n, n - 1) in
    let closure_var : Expr.Local.var = "closure", 0 in
    let closure_args =
      let first_arg_field = 3 in
      List.init (n - 1) (fun i : Expr.t ->
          let field = first_arg_field + i in
          Struct_get
            { typ = closure_arg_typ;
              block = Expr.Var (Expr.Local.V closure_var);
              field = Expr.I32 (Int32.of_int field)
            })
    in
    let args = closure_args @ [Expr.Var param_arg; Expr.Var env_arg] in
    let func : Expr.t =
      Struct_get
        { typ = closure_arg_typ;
          block = Expr.Var (Expr.Local.V closure_var);
          field = Expr.I32 2l
        }
    in
    Expr.let_ closure_var
      (Ref_cast { typ = closure_arg_typ; r = Var env_arg })
      (Expr.Call_ref { typ = Type.Var.Func { arity = n }; args; func })

  let caml_curry_alloc ~param_arg ~env_arg n m : Expr.t =
    (* arity, func, env, arg1..., argn-1, argn *)
    let closure_arg_typ = Type.Var.Partial_closure (n, m) in
    let closure_var : Expr.Local.var = "closure", 0 in
    let closure_args =
      let first_arg_field = 3 in
      List.init m (fun i : Expr.t ->
          let field = first_arg_field + i in
          Struct_get
            { typ = closure_arg_typ;
              block = Expr.Var (Expr.Local.V closure_var);
              field = Expr.I32 (Int32.of_int field)
            })
    in
    let fields =
      [ Expr.I32 1l;
        Expr.Ref_func (Caml_curry (n, m + 1));
        Expr.Var (Expr.Local.V closure_var) ]
      @ closure_args @ [Expr.Var param_arg]
    in
    let body : Expr.t =
      Struct_new (Type.Var.Partial_closure (n, m + 1), fields)
    in
    if m = 0
    then body
    else
      Expr.let_ closure_var
        (Ref_cast { typ = closure_arg_typ; r = Var env_arg })
        body

  let caml_curry n m =
    let param_arg = Param.P ("arg", 0) in
    let env_arg = Param.Env in
    let body =
      if m = n - 1
      then
        caml_curry_apply ~param_arg:(Expr.Local.Param param_arg)
          ~env_arg:(Expr.Local.Param env_arg) n
      else
        caml_curry_alloc ~param_arg:(Expr.Local.Param param_arg)
          ~env_arg:(Expr.Local.Param env_arg) n m
    in
    Func.Decl
      { params = [param_arg, Type.Any; env_arg, Type.Rvar Type.Var.Env];
        result = Some Type.Any;
        body
      }

  let make_common required_arities =
    Arity.Set.fold
      (fun arity decls ->
        let ms = List.init arity (fun i -> i) in
        List.fold_left
          (fun decls applied_args ->
            let decl =
              Decl.Func
                { name = Func_id.Caml_curry (arity, applied_args);
                  descr = caml_curry arity applied_args
                }
            in
            decl :: decls)
          decls ms)
      (Arity.Set.remove 1 required_arities)
      []
end

module ToWasm = struct
  module Cst = struct
    type k =
      | V
      | Hov

    type t =
      | Int of int64
      | Atom of string
      | Node of
          { name : string;
            args : t list;
            k : k;
            force_paren : bool
          }

    let print_lst f ppf l =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
        f ppf l

    let rec emit ppf = function
      | Int i -> Format.fprintf ppf "%Li" i
      | Atom s -> Format.pp_print_string ppf s
      | Node { name; args; k; force_paren } -> begin
        match args with
        | [] ->
          if force_paren
          then Format.fprintf ppf "(%s)" name
          else Format.pp_print_string ppf name
        | _ ->
          (match k with
          | V -> Format.fprintf ppf "@[<v 2>"
          | Hov -> Format.fprintf ppf "@[<hov 2>");
          Format.fprintf ppf "(%s@ %a)@]" name (print_lst emit) args
      end

    let nodev name args = Node { name; args; k = V; force_paren = false }
    let node name args = Node { name; args; k = Hov; force_paren = false }
    let node_p name args = Node { name; args; k = Hov; force_paren = true }
    let atom name = Atom name
  end
  module C = struct
    open Cst

    let ( !$ ) v = atom (Printf.sprintf "$%s" v)

    let global name typ descr =
      node "global" [!$ name; typ; descr]

    let reft name =
      node "ref" [ !$ name]

    let struct_new_canon typ fields =
      node "struct.new_canon"
        (( !$ typ ) :: fields)

    let i32_ i = node "i32.const" [Int (Int64.of_int i)]
    let i32 i = node "i32.const" [Int (Int64.of_int32 i)]
    let i31_new i = node "i31.new" [Int (Int64.of_int i)]

    let ref_func f = node "ref.func" [ !$ (Func_id.name f) ]
    let global_get g = node "global.get" [ !$ (Global.name g) ]

    let module_ m = node "module" m
  end

  let tvar v = Type.Var.name v
  let gvar v = Global.name v

  let conv_const name (Const.Struct { typ; fields }) =
    let field (field : Const.field) : Cst.t =
      match field with
      | I8 i | I16 i -> C.i32_ i
      | I31 i -> C.i31_new i
      | Ref_func f -> C.ref_func f
      | Global g -> C.global_get g
    in
    C.global
      (Global.name name)
      (C.reft (tvar typ))
      (C.struct_new_canon (tvar typ) (List.map field fields))

  let rec conv_decl = function
    | [] -> []
    | Decl.Const { name; descr } :: tl ->
        conv_const name descr :: conv_decl tl
    | Decl.Func _ :: tl ->
        conv_decl tl
    | Decl.Type _ :: tl ->
        conv_decl tl

  let conv_module module_ =
    C.module_ (conv_decl module_)
end

let run ~output_prefix (flambda : Flambda.program) =
  State.reset ();
  let m = Conv.conv_body flambda.program_body in
  let wasm = ToWasm.conv_module m in
  Format.printf "WASM %s@.%a@." output_prefix Module.print m;
  Format.printf "@.%a@."ToWasm.Cst.emit wasm;
  let common = Conv.make_common !State.arities in
  Format.printf "COMMON@.%a@." Module.print common
