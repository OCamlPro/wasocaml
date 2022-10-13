[@@@ocaml.warning "-23"]
[@@@ocaml.warning "-32-60"]

module Var = struct
  type t =
    { name : string;
      id : int
    }

  module M = Map.Make (String)
  type env = int M.t

  let fresh ?(name = "v") env =
    let n = match M.find_opt name env with None -> 0 | Some i -> i + 1 in
    let env = M.add name n env in
    { name; id = n }, env

  let s { name; id } =
    if id = 0 then "$" ^ name else Printf.sprintf "$%s_%i" name id

  let fresh_symbol s env =
    let linkage_name = Symbol.label s in
    let name = Linkage_name.to_string linkage_name in
    fresh ~name env

  let fresh_var v env =
    let name = Variable.name v in
    fresh ~name env

  let fresh_mutable_var v env =
    let name = Mutable_variable.name v in
    fresh ~name env

  let empty = M.empty
  let env, empty = fresh ~name:"env" empty
  let gen_env, empty = fresh ~name:"gen_env" empty
  let arity_field, empty = fresh ~name:"arity" empty
  let default_fun, empty = fresh ~name:"fun" empty

  (* TODO remove, but lazy right now *)
  let name s = { name = s; id = 0 }
end

module Cst = struct
  type k =
    | V
    | Hov

  type t =
    | Int of int64
    | Node of
        { name : string;
          args : t list;
          k : k;
          force_paren : bool
        }

  let print_lst f ppf l =
    Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ") f ppf l

  let rec emit ppf = function
    | Int i -> Format.fprintf ppf "%Li" i
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

  let int i = Int (Int64.of_int i)
  let nodev name args = Node { name; args; k = V; force_paren = false }
  let node name args = Node { name; args; k = Hov; force_paren = false }
  let node_p name args = Node { name; args; k = Hov; force_paren = true }
  let atom name = Node { name; args = []; k = Hov; force_paren = false }

  let wasm_var v = atom (Var.s v)

  let module_ fields = nodev "module" fields

  let elem_declare_fun ~name =
    node "elem" [atom "declare"; atom "func"; wasm_var name]

  let struct_descr fields =
    let fields =
      List.map
        (fun (name, typ) ->
          match name with
          | None -> node "field" [typ]
          | Some name -> node "field" [wasm_var name; typ])
        fields
    in
    node_p "struct" fields

  let struct_type ~name fields = node "type" [wasm_var name; struct_descr fields]

  let struct_subtype ~name ~sub fields =
    node "type" [wasm_var name; node "sub" [wasm_var sub; struct_descr fields]]

  let func_type ~name ~params ~result =
    let params = List.map (fun typ -> node "param" [typ]) params in
    node "type" [wasm_var name; node "func" (params @ [node "result" [result]])]

  let func ~name ~params ~result ~locals ~body =
    let params =
      List.map (fun (name, typ) -> node "param" [wasm_var name; typ]) params
    in
    let result = node "result" [result] in
    let locals =
      List.map (fun (name, typ) -> node "local" [wasm_var name; typ]) locals
    in
    let fields = (wasm_var name :: params) @ (result :: (locals @ body)) in
    node "func" fields

  let ref_type arg = node "ref" [wasm_var arg]

  module Local = struct
    let set ~name = node "local.set" [wasm_var name]

    let get ~name = node "local.get" [wasm_var name]
  end

  module Cast = struct
    let i31 arg = node "ref.cast" [atom "i31"; arg]
  end
end

(* let mk_const _env _acc const = [] *)

let ref_any = Cst.(node "ref" [atom "any"])
let ref_env = Cst.(node "ref" [Cst.wasm_var Var.env])

module Arity = struct
  module M = struct
    type t = int
    let compare = Int.compare
  end
  include M
  module Set = Set.Make (M)
  module Map = Map.Make (M)
end

module Env = struct
  type t =
    { venv : Var.env;
      symbol_names : Var.t Symbol.Map.t;
      variable_names : Var.t Variable.Map.t;
      mutable_variable_names : Var.t Mutable_variable.Map.t
    }

  let empty =
    { venv = Var.empty;
      symbol_names = Symbol.Map.empty;
      variable_names = Variable.Map.empty;
      mutable_variable_names = Mutable_variable.Map.empty
    }

  let new_symbol_name symbol t =
    let name, venv = Var.fresh_symbol symbol t.venv in
    let symbol_names = Symbol.Map.add symbol name t.symbol_names in
    name, { t with venv; symbol_names }

  let new_variable_name var t =
    let name, venv = Var.fresh_var var t.venv in
    let variable_names = Variable.Map.add var name t.variable_names in
    name, { t with venv; variable_names }

  let new_mutable_variable_name var t =
    let name, venv = Var.fresh_mutable_var var t.venv in
    let mutable_variable_names =
      Mutable_variable.Map.add var name t.mutable_variable_names
    in
    name, { t with venv; mutable_variable_names }

  let var var env = Variable.Map.find var env.variable_names
end

module Downard_acc = struct
  type t =
    { venv : Var.env;
      func_types : Var.t Arity.Map.t;
      closure_types : (Var.t Arity.Map.t * Var.t) Arity.Map.t
    }

  let empty =
    { venv = Var.empty;
      func_types = Arity.Map.empty;
      closure_types = Arity.Map.empty
    }

  let add_arity i t =
    match Arity.Map.find i t.func_types with
    | exception Not_found ->
      let name, venv =
        Var.fresh ~name:(Printf.sprintf "func_type_%i" i) t.venv
      in
      let t = { t with venv; func_types = Arity.Map.add i name t.func_types } in
      name, t
    | name -> name, t

  let add_closure_type ~arity ~fields t =
    let var () =
      Var.fresh ~name:(Printf.sprintf "closure_type_%i_%i" arity fields) t.venv
    in
    match Arity.Map.find arity t.closure_types with
    | exception Not_found ->
      let name, venv = var () in
      let name_base, venv =
        Var.fresh ~name:(Printf.sprintf "closure_type_%i" arity) venv
      in
      let t =
        { t with
          venv;
          closure_types =
            Arity.Map.add arity
              (Arity.Map.singleton fields name, name_base)
              t.closure_types
        }
      in
      name, t
    | sizes, base_name -> (
      match Arity.Map.find fields sizes with
      | exception Not_found ->
        let name, venv = var () in
        let sizes = Arity.Map.add fields name sizes in
        let t =
          { t with
            venv;
            closure_types =
              Arity.Map.add arity (sizes, base_name) t.closure_types
          }
        in
        name, t
      | name -> name, t)
end

module Upward_acc = struct
  type t = { dacc : Downard_acc.t }
  let of_dacc dacc = { dacc }
end

module DA = Downard_acc
module UA = Upward_acc

let partial_closure_name ~applied ~total =
  Var.name (Printf.sprintf "closure_apply_%i_%i" applied total)

(* type struct (arity fun1 closurem field1 .. fieldn) *)
let apply_closure (dacc : DA.t) ~closure_n ~applied ~total =
  let func_type_1, dacc = DA.add_arity 1 dacc in
  let name = partial_closure_name ~applied ~total in
  let fields = List.init applied (fun _ -> None, ref_any) in
  let fields =
    [ None, Cst.atom "i8";
      None, Cst.ref_type func_type_1;
      None, Cst.ref_type closure_n ]
    @ fields
  in
  let decl = Cst.struct_subtype ~name ~sub:Var.gen_env fields in
  decl, dacc

(* let parital_apply_fun' (env : Env.t) (dacc : DA.t) ~applied ~total = *)

let params (env : Env.t) params =
  let params, env =
    List.fold_left
      (fun (params, dacc) param ->
        let name, env = Env.new_variable_name (Parameter.var param) dacc in
        (name, ref_any) :: params, env)
      ([], env) params
  in
  List.rev params, env

let bound_vars fl =
  let add var (vars, mut_vars) = Variable.Set.add var vars, mut_vars in
  let add_mut var (vars, mut_vars) =
    vars, Mutable_variable.Set.add var mut_vars
  in
  let rec loop acc (fl : Flambda.t) =
    match fl with
    | Let { var; body; defining_expr } ->
      let acc = loop_named acc defining_expr in
      loop (add var acc) body
    | Let_mutable { var; body; _ } -> loop (add_mut var acc) body
    | Let_rec (bindings, body) ->
      let acc =
        List.fold_left
          (fun acc (var, defining_expr) ->
            let acc = loop_named acc defining_expr in
            let acc = add var acc in
            acc)
          acc bindings
      in
      loop acc body
    | Static_catch (_, vars, body, handler) ->
      let acc = List.fold_left (fun acc var -> add var acc) acc vars in
      let acc = loop acc handler in
      loop acc body
    (* | Try_with of t * Variable.t * t
     * | While of t * t
     * | For of for_loop
     * 
     * | If_then_else of Variable.t * t * t
     * | Switch of Variable.t * switch
     * | String_switch of Variable.t * (string * t) list * t option *)
    | Var _ | Apply _ | Send _ | Assign _ | Static_raise _ | Proved_unreachable
      ->
      acc
    | _ -> assert false
  and loop_named acc (named : Flambda.named) =
    match named with Expr e -> loop acc e | _ -> acc
  in
  loop (Variable.Set.empty, Mutable_variable.Set.empty) fl

let rec conv_body env dacc (expr : Flambda.program_body) : Cst.t list * UA.t =
  match expr with
  | Let_symbol (symbol, Set_of_closures set, body) ->
    let _name, env = Env.new_symbol_name symbol env in
    let decl, dacc = function_declarations env dacc set.function_decls in
    let body, uacc = conv_body env dacc body in
    decl @ body, uacc
  | Let_symbol (_symbol, _const, body) ->
    Format.printf "IGNORE LET SYMBOL@.";
    conv_body env dacc body
  | Let_rec_symbol (_, body) ->
    Format.printf "IGNORE LET REC SYMBOL@.";
    conv_body env dacc body
  | Initialize_symbol (_, _, _, body) ->
    Format.printf "IGNORE INITIALIZE SYMBOL@.";
    conv_body env dacc body
  | Effect (_, body) ->
    Format.printf "IGNORE EFFECT@.";
    conv_body env dacc body
  | End _end_symbol -> [], UA.of_dacc dacc

and function_declarations env dacc
    (declarations : Flambda.function_declarations) : Cst.t list * DA.t =
  Variable.Map.fold
    (fun name declaration (expr, dacc) ->
      let e, dacc = function_declaration env dacc name declaration in
      e @ expr, dacc)
    declarations.funs ([], dacc)

and function_declaration (env : Env.t) (dacc : DA.t) name
    (declaration : Flambda.function_declaration) =
  let function_symbol = Compilenv.function_label (Closure_id.wrap name) in
  (* let name, dacc = DA.new_symbol_name symbol dacc in *)
  let func_name, _XXXenv = Var.fresh ~name:function_symbol env.venv in
  let params, env = params env declaration.params in
  let params = params @ [Var.env, ref_env] in
  let arity = List.length declaration.params in
  let _type_name, dacc = DA.add_arity arity dacc in
  let _closure_type_name, dacc =
    let param_set =
      List.fold_left
        (fun param_set param ->
          Variable.Set.add (Parameter.var param) param_set)
        Variable.Set.empty declaration.params
    in
    let actually_free =
      Variable.Set.diff declaration.free_variables param_set
    in
    DA.add_closure_type ~arity
      ~fields:(Variable.Set.cardinal actually_free)
      dacc
  in
  let decl = Cst.elem_declare_fun ~name:func_name in
  let locals, env =
    let all_bound_variables, all_bound_mutable_variables =
      bound_vars declaration.body
    in
    let locals, env =
      Variable.Set.fold
        (fun var (locals, env) ->
          let name, env = Env.new_variable_name var env in
          let local = name, ref_any in
          local :: locals, env)
        all_bound_variables ([], env)
    in
    let locals, env =
      Mutable_variable.Set.fold
        (fun var (locals, env) ->
          let name, env = Env.new_mutable_variable_name var env in
          let local = name, ref_any in
          local :: locals, env)
        all_bound_mutable_variables (locals, env)
    in
    locals, env
  in
  let body, dacc = conv_expr env dacc declaration.body in
  let cst = Cst.func ~name:func_name ~params ~result:ref_any ~locals ~body in
  [decl; cst], dacc

and conv_expr env dacc (expr : Flambda.t) : Cst.t list * DA.t =
  match expr with
  | Let { var; defining_expr; body; _ } ->
    let named, dacc = conv_named env dacc defining_expr in
    let set = Cst.Local.set ~name:(Env.var var env) in
    let body, dacc = conv_expr env dacc body in
    named @ (set :: body), dacc
  | Var var -> [Cst.Local.get ~name:(Env.var var env)], dacc
  | _ -> failwith "TODO"

and conv_named env dacc (named : Flambda.named) : Cst.t list * DA.t =
  match named with
  | Prim (prim, args, _dbg) ->
    let cst = conv_prim env dacc ~prim ~args in
    cst, dacc
  | _ -> failwith "TODO"

and conv_prim env _dacc ~(prim : Clambda_primitives.primitive) ~args :
    Cst.t list =
  let args = List.map (fun var -> Cst.Local.get ~name:(Env.var var env)) args in
  let i32 v = Cst.node "i31.get_s" [Cst.Cast.i31 v] in
  let i31 v = Cst.node "i31.new" [v] in
  match prim with
  | Paddint -> [i31 (Cst.node "i32.add" (List.map i32 args))]
  | Psubint -> [i31 (Cst.node "i32.sub" (List.map i32 args))]
  | _ -> failwith "TODO"

let func_types (dacc : DA.t) =
  Arity.Map.fold
    (fun arity name decls ->
      let params = List.init arity (fun _ -> ref_any) in
      let decl =
        Cst.func_type ~name ~params:(params @ [ref_env]) ~result:ref_any
      in
      decl :: decls)
    dacc.func_types []

let closure_types (dacc : DA.t) =
  let func_type_1, dacc = DA.add_arity 1 dacc in
  let mk_decl ~name ~sub ~arity ~size =
    let fields = List.init size (fun _ -> None, ref_any) in
    let func_type_name = Arity.Map.find arity dacc.func_types in
    let fields =
      [ None, Cst.atom "i8";
        None, Cst.ref_type func_type_1;
        None, Cst.ref_type func_type_name ]
      @ fields
    in
    let decl = Cst.struct_subtype ~name ~sub fields in
    decl
  in
  let closure_types =
    Arity.Map.fold
      (fun arity (sizes, base_name) decls ->
        let base_decl =
          mk_decl ~name:base_name ~arity ~sub:Var.gen_env ~size:0
        in
        let decls =
          Arity.Map.fold
            (fun size name decls ->
              let decl = mk_decl ~name ~arity ~sub:base_name ~size in
              decl :: decls)
            sizes decls
        in
        base_decl :: decls)
      dacc.closure_types []
  in
  closure_types, dacc

let partial_apply_closure_types (dacc : DA.t) =
  let closure_types, dacc =
    Arity.Map.fold
      (fun arity (_sizes, base_name) (decls, dacc) ->
        let dacc = ref dacc in
        let decls' =
          List.init (arity - 1) (fun i ->
              let applied = i + 1 in
              let decl, dacc' =
                apply_closure !dacc ~closure_n:base_name ~applied ~total:arity
              in
              dacc := dacc';
              decl)
        in
        decls' @ decls, !dacc)
      dacc.closure_types ([], dacc)
  in
  closure_types, dacc

let types dacc =
  let func_type_1, dacc = DA.add_arity 1 dacc in
  let env_type = Cst.struct_type ~name:Var.env [] in
  let gen_env_type =
    Cst.struct_subtype ~name:Var.gen_env ~sub:Var.env
      [ Some Var.arity_field, Cst.atom "i8";
        Some Var.default_fun, Cst.ref_type func_type_1 ]
  in
  let closure_types, dacc = closure_types dacc in
  let partial_apply_closure_types, dacc = partial_apply_closure_types dacc in
  let func_types = func_types dacc in
  env_type
  :: (List.rev func_types
     @ (gen_env_type :: (closure_types @ partial_apply_closure_types)))

let module_ (flambda : Flambda.program) =
  let dacc = DA.empty in
  let declarations, uacc = conv_body Env.empty dacc flambda.program_body in
  let types = types uacc.dacc in
  Cst.module_ (types @ declarations)

let emit ~output_prefix (flambda : Flambda.program) =
  Wtyp.run ~output_prefix flambda;
  let wastfile = output_prefix ^ ".wast" in
  let oc = open_out_bin wastfile in
  let ppf = Format.formatter_of_out_channel oc in
  Misc.try_finally
    ~always:(fun () ->
      Format.fprintf ppf "@.";
      close_out oc)
    (* ~exceptionally:(fun () -> Misc.remove_file wastfile) *)
      (fun () ->
      let cst = module_ flambda in
      Cst.emit ppf cst)
