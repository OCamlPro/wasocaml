[@@@ocaml.warning "-23"]
[@@@ocaml.warning "-37"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-60"]
[@@@ocaml.warning "-69"]

let print_list f sep ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "%s@ " sep)
    f ppf l

module MSet(M : Set.OrderedType) = struct
  include Set.Make(M)
  let (+=) r v = r := add v !r
end

module Arity = struct
  type t = int
  module Set = MSet (Int)
end
module Closure_type = struct
  module M = struct
    type t = { arity : int; fields : int }
    let compare = compare
  end
  include M
  module Set = MSet (M)
end

module State = struct
  let arities = ref Arity.Set.empty
  let block_sizes = ref Arity.Set.empty
  let closure_types = ref Closure_type.Set.empty

  let add_arity i =
    Arity.Set.(arities += i)
  let add_block_size i =
    Arity.Set.(block_sizes += i)
  let add_closure_type ~arity ~fields =
    Closure_type.Set.(closure_types += { arity; fields })

  let reset () =
    arities := Arity.Set.empty;
    block_sizes := Arity.Set.empty;
    closure_types := Closure_type.Set.empty
end

module Type = struct
  module Var = struct
    type t =
      | V of string * int
      | Partial_closure of int * int
      | Func of { arity : int }
      | Closure of
          { arity : int;
            fields : int
          }
      | Gen_closure of { arity : int }
      | Env
      | Block of { size : int }
      | Gen_block
      | I31

    let name = function
      | V (name, n) -> Format.asprintf "$%s_%i" name n
      | Partial_closure (n, m) -> Format.asprintf "$Partial_closure_%i_%i" n m
      | Env -> Format.asprintf "$Env"
      | Func { arity } -> Format.asprintf "$Func_%i" arity
      | Block { size } -> Format.asprintf "$Block_%i" size
      | Gen_block -> Format.asprintf "$Gen_block"
      | Closure { arity; fields } ->
        Format.asprintf "$Closure_%i_%i" arity fields
      | Gen_closure { arity } ->
          Format.asprintf "$Gen_closure_%i" arity
      | I31 -> "i31"

    let print ppf v = Format.pp_print_string ppf (name v)
  end

  type atom =
    | I8
    | I16
    | Any
    | Rvar of Var.t

  type descr =
    | Struct of {
        sub : Var.t option;
        fields : atom list;
      }
    | Func of
        { params : atom list;
          result : atom option
        }

  let print_atom ppf = function
    | I8 -> Format.fprintf ppf "i8"
    | I16 -> Format.fprintf ppf "i16"
    | Any -> Format.fprintf ppf "ref_any"
    | Rvar v -> Format.fprintf ppf "ref_%a" Var.print v

  let print_descr ppf = function
    | Struct { sub; fields = atoms } ->
        let pp_sub ppf = function
          | None -> ()
          | Some sub ->
              Format.fprintf ppf "sub: %a;@ " Var.print sub
        in
      Format.fprintf ppf "@[<hov 2>Struct {%a%a}@]"
        pp_sub sub
        (print_list print_atom ";")
        atoms
    | Func { params; result = None } ->
      Format.fprintf ppf "@[<hov 2>Func {%a}@]" (print_list print_atom ",") params
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
    module Set = Set.Make (M)
    module Map = Map.Make (M)
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
          typ : Type.atom;
          defining_expr : t;
          body : t
        }
    | Binop of binop * (t * t)
    | Unop of unop * t
    | Struct_new of Type.Var.t * t list
    | Struct_get of
        { typ : Type.Var.t;
          block : t;
          field : int
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
      Format.fprintf ppf "@[<hov 2>Struct_get(%a:@ %a).(%i)@]" Type.Var.print
        typ print block field
    | Call_ref { typ; args; func } ->
      Format.fprintf ppf "@[<hov 2>Call_ref(%a:@ %a(%a))@]" Type.Var.print typ
        print func (print_list print ",") args
    | Ref_cast { typ; r } ->
      Format.fprintf ppf "@[<hov 2>Ref_cast(%a:@ %a)@]" Type.Var.print typ print
        r

  let let_ var typ defining_expr body = Let { var; typ; defining_expr; body }

  let required_locals expr =
    let rec loop acc = function
      | Var _ | I32 _ | Ref_func _ -> acc
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
      | Struct_get { typ = _; block; field = _ } ->
        loop acc block
      | Call_ref { typ = _; args; func } ->
        List.fold_left (fun acc arg -> loop acc arg) (loop acc func) args
      | Ref_cast { typ = _; r } -> loop acc r
    in
    loop Local.Map.empty expr
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
    | Type_rec of (Type.Var.t * Type.descr) list
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
    | Type_rec l ->
        let pp ppf (var, descr) =
          Format.fprintf ppf "(%a = %a)" Type.Var.print var Type.print_descr
            descr
        in
        Format.fprintf ppf "type_rec %a"
          (print_list pp "") l
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
    let size = List.length fields in
    State.add_block_size size;
    let field (f : Flambda.constant_defining_value_block_field) : Const.field =
      match f with
      | Symbol s -> Global (Global.of_symbol s)
      | Const (Int i) -> I31 i
      | Const (Char c) -> I31 (Char.code c)
    in
    let fields =
      [Const.I8 (Tag.to_int tag); Const.I16 size]
      @ List.map field fields
    in
    Struct { typ = Type.Var.Block { size }; fields }

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
        (* let closure_name = Global.Closure name in *)
        let closure_name =
          let closure_symbol = Compilenv.closure_symbol (Closure_id.wrap name) in
          Global.of_symbol closure_symbol
        in
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
        State.add_closure_type ~arity ~fields:0;
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
      Let { var = local; typ = Type.Any; defining_expr; body }
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
    let i32 v = Expr.Unop (I31_get_s, Ref_cast { typ = I31; r = v }) in
    let i31 v = Expr.Unop (I31_new, v) in
    match prim with
    | Paddint -> i31 (Expr.Binop (I32_add, args2 (List.map i32 args)))
    | Psubint -> i31 (Expr.Binop (I32_sub, args2 (List.map i32 args)))
    | _ -> failwith "TODO"

  let block_type size : Type.descr =
    let fields = List.init size (fun _ -> Type.Any) in
    Struct { sub = Some Gen_block;
             fields =
      (* Tag *)
      I8 ::
      (* size *)
      I16 ::
      fields }

  let gen_closure_type ~arity : Type.descr =
    let head : Type.atom list =
      if arity = 1 then
        [ I8 (* arity *) ;
          Rvar (Func { arity = 1 }) (* generic func *);
        ]
      else
        [ I8 (* arity *) ;
          Rvar (Func { arity = 1 }) (* generic func *);
          Rvar (Func { arity }) (* direct call func *);
        ]
    in
    Struct { sub = Some Env; fields = head }

  let closure_type ~arity ~fields : Type.descr =
    let head : Type.atom list =
      if arity = 1 then
        [ I8 (* arity *) ;
          Rvar (Func { arity = 1 }) (* generic func *);
        ]
      else
        [ I8 (* arity *) ;
          Rvar (Func { arity = 1 }) (* generic func *);
          Rvar (Func { arity }) (* direct call func *);
        ]
    in
    let fields = List.init fields (fun _ -> Type.Any) in
    Struct { sub = Some (Gen_closure { arity }); fields = head @ fields }

  let partial_closure_type ~arity ~applied : Type.descr =
    let args = List.init applied (fun _ -> Type.Any) in
    let fields : Type.atom list =
      [ Type.I8 (* arity *) ;
        Type.Rvar (Func { arity = 1 }) (* generic func *);
        Type.Rvar (Gen_closure { arity }) ] @ args
    in
    Struct { sub = Some Env; fields = fields }

  let func_type size : Type.descr =
    let params =
      List.init size (fun _ -> Type.Any)
    in
    Func { params = params @ [Type.Rvar Env];
           result = Some Any }

  let caml_curry_apply ~param_arg ~env_arg n =
    let partial_closure_arg_typ = Type.Var.Partial_closure (n, n - 1) in
    let partial_closure_var : Expr.Local.var = "partial_closure", 0 in
    let closure_arg_typ = Type.Var.Gen_closure { arity = n } in
    let closure_var : Expr.Local.var = "closure", 0 in
    let closure_args =
      let first_arg_field = 3 in
      List.init (n - 1) (fun i : Expr.t ->
          let field = first_arg_field + i in
          Struct_get
            { typ = partial_closure_arg_typ;
              block = Expr.Var (Expr.Local.V partial_closure_var);
              field = field
            })
    in
    let args = closure_args @ [Expr.Var param_arg; Expr.Var env_arg] in
    let func : Expr.t =
      Struct_get
        { typ = closure_arg_typ;
          block = Expr.Var (Expr.Local.V closure_var);
          field = 2
        }
    in
    Expr.let_ partial_closure_var (Type.Rvar partial_closure_arg_typ)
      (Ref_cast { typ = partial_closure_arg_typ; r = Var env_arg })
      (Expr.let_ closure_var (Type.Rvar closure_arg_typ)
         (Struct_get
            { typ = partial_closure_arg_typ;
              block = Expr.Var (Expr.Local.V partial_closure_var);
              field = 2
            })
         (Expr.Call_ref { typ = Type.Var.Func { arity = n }; args; func }))

  let caml_curry_alloc ~param_arg ~env_arg n m : Expr.t =
    (* arity, func, env, arg1..., argn-1, argn *)
    let closure_arg_typ = Type.Var.Partial_closure (n, m) in
    let closure_var : Expr.Local.var = "closure", 0 in
    let closure_local =
      Expr.Local.V closure_var
    in
    let closure_args =
      let first_arg_field = 3 in
      List.init m (fun i : Expr.t ->
          let field = first_arg_field + i in
          Struct_get
            { typ = closure_arg_typ;
              block = Expr.Var closure_local;
              field = field
            })
    in
    let closure_field =
      if m = 0 then
        (Expr.Ref_cast { typ = Type.Var.Gen_closure { arity = n }; r = Var env_arg })
      else
        Expr.Struct_get
          { typ = closure_arg_typ;
            block = Expr.Var closure_local;
            field = 2
          }
    in
    let fields =
      [ Expr.I32 1l;
        Expr.Ref_func (Caml_curry (n, m + 1));
        closure_field ]
      @ closure_args @ [Expr.Var param_arg]
    in
    let body : Expr.t =
      Struct_new (Type.Var.Partial_closure (n, m + 1), fields)
    in
    if m = 0
    then body
    else
      Expr.let_ closure_var (Type.Rvar closure_arg_typ)
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

  let func_1_and_env =
    let env =
      let fields : Type.atom list =
        [ I8 (* arity *) ;
          Rvar (Func { arity = 1 }) (* generic func *);
        ]
      in
      (Type.Var.Env, Type.Struct { sub = None; fields })
    in
    let func_1 =
      let name = Type.Var.Func { arity = 1 } in
      let descr = func_type 1 in
      (name, descr)
    in
    Decl.Type_rec [func_1; env]

  let gen_block =
    let fields : Type.atom list =
      [ I8 (* tag *) ;
        I16 (* size *) ;
      ]
    in
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
                { name = Func_id.Caml_curry (arity, applied_args);
                  descr = caml_curry arity applied_args
                }
            in
            decl :: decls)
          decls ms)
      (Arity.Set.remove 1 !State.arities)
      decls
    in
    let decls =
      Arity.Set.fold
        (fun size decls ->
           let name = Type.Var.Block { size } in
           let descr = block_type size in
           Decl.Type (name, descr) :: decls
        ) !State.block_sizes decls
    in
    let decls =
      Arity.Set.fold
      (fun arity decls ->
        let ms = List.init arity (fun i -> i) in
        List.fold_left
          (fun decls applied_args ->
            let decl =
              Decl.Type (Type.Var.Partial_closure (arity, applied_args),
                         partial_closure_type ~arity ~applied:applied_args)
            in
            decl :: decls)
          decls ms)
      (Arity.Set.remove 1 !State.arities)
      decls
    in
    let decls =
      Closure_type.Set.fold
        (fun { arity; fields } decls ->
           let name = Type.Var.Closure { arity; fields } in
           let descr = closure_type ~arity ~fields in
           Decl.Type (name, descr) :: decls
        ) !State.closure_types decls
    in
    let decls =
      Arity.Set.fold
        (fun arity decls ->
           let name = Type.Var.Gen_closure { arity } in
           let descr = gen_closure_type ~arity in
           Decl.Type (name, descr) :: decls
        ) !State.arities decls
    in
    let decls = gen_block :: decls in
    let decls =
      Arity.Set.fold
        (fun arity decls ->
           let name = Type.Var.Func { arity } in
           let descr = func_type arity in
           Decl.Type (name, descr) :: decls
        ) (Arity.Set.remove 1 !State.arities) decls
    in
    let decls = func_1_and_env :: decls in
    decls
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
            args_h : t list;
            args_v : t list;
            force_paren : bool
          }

    let print_lst f ppf l =
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
        f ppf l

    let rec emit ppf = function
      | Int i -> Format.fprintf ppf "%Li" i
      | Atom s -> Format.pp_print_string ppf s
      | Node { name; args_h; args_v; force_paren } -> begin
        match args_h, args_v with
        | [], [] ->
          if force_paren
          then Format.fprintf ppf "(%s)" name
          else Format.pp_print_string ppf name
        | _ ->
          Format.fprintf ppf "@[<v 2>@[<hov 2>";
          Format.fprintf ppf "(%s@ %a@]" name (print_lst emit) args_h;
          (match args_v with
           | [] -> ()
           | _ ->
               Format.fprintf ppf "@ %a" (print_lst emit) args_v);
          Format.fprintf ppf ")@]"
      end

    let nodev name args = Node { name; args_h = []; args_v = args; force_paren = false }
    let nodehv name args_h args_v = Node { name; args_h; args_v; force_paren = false }
    let node name args = Node { name; args_h = args; args_v = []; force_paren = false }
    let node_p name args = Node { name; args_h = args; args_v = []; force_paren = true }
    let atom name = Atom name
  end
  module C = struct
    open Cst

    let ( !$ ) v = atom (Printf.sprintf "$%s" v)

    let type_name v = atom (Type.Var.name v)

    let global name typ descr = node "global" [!$name; typ; descr]

    let reft name = node "ref" [type_name name]

    let struct_new_canon typ fields = node "struct.new_canon" (type_name typ :: fields)

    let int i = Int (Int64.of_int i)

    let i32_ i = node "i32.const" [int i]
    let i32 i = node "i32.const" [Int (Int64.of_int32 i)]
    let i31_new i = node "i31.new" [int i]

    let ref_func f = node "ref.func" [!$(Func_id.name f)]
    let global_get g = node "global.get" [!$(Global.name g)]

    let local_get l = node "local.get" [!$(Expr.Local.name l)]
    let local_set l = node "local.set" [!$(Expr.Local.name l)]

    let struct_get typ field = node "struct.get" [type_name typ; int field]
    let call_ref typ = node "call_ref" [type_name typ]
    let ref_cast typ = node "ref.cast" [type_name typ]

    let declare_func f =
      node "elem" [atom "declare"; atom "func"; !$(Func_id.name f)]

    let type_atom (t : Type.atom) =
      match t with
      | I8 -> atom "i8"
      | I16 -> atom "i16"
      | Any -> node "ref" [atom "any"]
      | Rvar v -> reft v

    let local l t = node "local" [!$(Expr.Local.var_name l); type_atom t]
    let param p t = node "param" [!$(Param.name p); type_atom t]
    let param_t t = node "param" [type_atom t]
    let result t = node "result" [type_atom t]

    let func ~name ~params ~result ~locals ~body =
      let fields =
        [!$(Func_id.name name)] @ (params @ result @ locals)
      in
      nodehv "func" fields body

    let field f = node "field" [type_atom f]

    let struct_type fields = node "struct" (List.map field fields)
    let func_type params res =
      let res =
        match res with
        | None -> []
        | Some res -> [result res]
      in
      node "func" ((List.map param_t params) @ res)

    let type_ name descr = node "type" [ (type_name name); descr ]
    let sub name descr = node "sub" [ (type_name name); descr ]

    let rec_ l = node "rec" l

    let module_ m = nodev "module" m
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
    C.global (Global.name name)
      (C.reft typ)
      (C.struct_new_canon typ (List.map field fields))

  let binop_name = function
    | Expr.I32_add -> "i32.add"
    | Expr.I32_sub -> "i32.sub"

  let unop_name = function
    | Expr.I31_get_s -> "i31.get_s"
    | Expr.I31_new -> "i31.new"

  let rec conv_expr (expr : Expr.t) =
    match expr with
    | Var v -> [C.local_get v]
    | Binop (op, (arg1, arg2)) ->
      conv_expr arg1 @ conv_expr arg2 @ [Cst.atom (binop_name op)]
    | Unop (op, arg) -> conv_expr arg @ [Cst.atom (unop_name op)]
    | Let { var; typ = _; defining_expr; body } ->
      conv_expr defining_expr
      @ (C.local_set (Expr.Local.V var) :: conv_expr body)
    | I32 i -> [C.i32 i]
    | Struct_new (typ, fields) ->
        let fields = List.map conv_expr fields in
        (List.flatten fields @
         [C.struct_new_canon typ []])
    | Ref_func fid ->
        [C.ref_func fid]
    | Struct_get { typ; block; field } ->
        conv_expr block @
        [C.struct_get typ field]
    | Call_ref { typ; args; func } ->
        List.flatten (List.map conv_expr args) @
        conv_expr func @
        [C.call_ref typ]
    | Ref_cast { typ; r } ->
        conv_expr r @
        [C.ref_cast typ]

  let conv_func name (func : Func.t) =
    match func with
    | Import _ ->
        Printf.printf "TODO CONV IMPORT@.";
        []
    | Decl { params; result; body } ->
      let func =
        let locals = Expr.required_locals body in
        let params = List.map (fun (p, t) -> C.param p t) params in
        let locals =
          Expr.Local.Map.fold (fun v t l -> C.local v t :: l) locals []
        in
        let body = conv_expr body in
        let result =
          match result with None -> [] | Some typ -> [C.result typ]
        in
        C.func ~name ~params ~locals ~result ~body
      in
      [C.declare_func name; func]

  let conv_type name (descr : Type.descr) =
    match descr with
    | Struct { sub; fields } ->
        let descr = C.struct_type fields in
        let descr =
          match sub with
          | None -> descr
          | Some sub -> C.sub sub descr
        in
        C.type_ name descr
    | Func { params; result } -> C.type_ name (C.func_type params result)

  let conv_type_rec types =
    C.rec_ (List.map (fun (name, descr) -> conv_type name descr) types)

  let rec conv_decl = function
    | [] -> []
    | Decl.Const { name; descr } :: tl -> conv_const name descr :: conv_decl tl
    | Decl.Func { name; descr } :: tl ->
      let func = conv_func name descr in
      func @ conv_decl tl
    | Decl.Type ( name, descr ) :: tl ->
        let type_ = conv_type name descr in
        type_ :: conv_decl tl
    | Decl.Type_rec types :: tl ->
        let type_ = conv_type_rec types in
        type_ :: conv_decl tl

  let conv_module module_ = C.module_ (conv_decl module_)
end

let run ~output_prefix (flambda : Flambda.program) =
  State.reset ();
  let m = Conv.conv_body flambda.program_body in
  Format.printf "WASM %s@.%a@." output_prefix Module.print m;
  let common =
    Conv.make_common ()
  in
  Format.printf "COMMON@.%a@." Module.print common;
  let wasm = ToWasm.conv_module (common @ m) in
  Format.printf "@.%a@." ToWasm.Cst.emit wasm;