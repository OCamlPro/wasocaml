(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type function_accessor =
  { field : int
  ; recursive_set : bool
  ; arity : int
  ; set : Set_of_closures_id.t
  }

type free_var_accessor =
  { field : int
  ; recursive_set : bool
  ; closure_size : int
  ; set : Set_of_closures_id.t
  }

type func =
  { arity : int
  ; fields : int
  }

type set_of_closures_id_type =
  { functions : func list
  ; fields : int
  }

type t =
  { function_accessors : function_accessor Closure_id.Map.t
  ; free_variable_accessors : free_var_accessor Var_within_closure.Map.t
  ; set_of_closures_id_types : set_of_closures_id_type Set_of_closures_id.Map.t
  }

let add_closure_offsets result ~constant
    ({ function_decls; free_vars } : Flambda.set_of_closures) =
  let is_recursive = Variable.Map.cardinal function_decls.funs > 1 in
  let closure_size = Variable.Map.cardinal free_vars in
  let assign_free_variable_offset var _ (map, pos) =
    let var_within_closure = Var_within_closure.wrap var in
    if Var_within_closure.Map.mem var_within_closure map then begin
      Misc.fatal_errorf
        "Closure_offsets.add_closure_offsets: free variable offset for %a \
         would be defined multiple times"
        Var_within_closure.print var_within_closure
    end;
    let accessor =
      { field = pos
      ; recursive_set = is_recursive
      ; closure_size
      ; set = function_decls.set_of_closures_id
      }
    in
    let map = Var_within_closure.Map.add var_within_closure accessor map in
    (map, pos + 1)
  in
  if not is_recursive then
    let fun_var, function_decl = Variable.Map.choose function_decls.funs in
    let closure_id = Closure_id.wrap fun_var in
    let arity = Flambda_utils.function_arity function_decl in
    let fun_offset =
      1
      +
        (* arity field *)
        if arity > 1 then 1 else 0
    in
    let fun_accessor =
      { field = fun_offset
      ; recursive_set = false
      ; arity
      ; set = function_decls.set_of_closures_id
      }
    in
    let function_accessors =
      Closure_id.Map.add closure_id fun_accessor result.function_accessors
    in
    let free_variable_accessors, _ =
      Variable.Map.fold assign_free_variable_offset free_vars
        (result.free_variable_accessors, fun_offset + 1)
    in
    { function_accessors
    ; free_variable_accessors
    ; set_of_closures_id_types = result.set_of_closures_id_types
    }
  else
    let assign_function_offset id function_decl (map, env_pos) =
      let env_pos = env_pos + 1 in
      let arity = Flambda_utils.function_arity function_decl in
      let closure_id = Closure_id.wrap id in
      if Closure_id.Map.mem closure_id map then begin
        Misc.fatal_errorf
          "Closure_offsets.add_closure_offsets: function offset for %a would \
           be defined multiple times"
          Closure_id.print closure_id
      end;
      let accessor =
        { field = env_pos
        ; set = function_decls.set_of_closures_id
        ; arity
        ; recursive_set = true
        }
      in
      let map = Closure_id.Map.add closure_id accessor map in
      (map, env_pos)
    in
    let function_accessors, last_allocated_slot =
      Variable.Map.fold assign_function_offset function_decls.funs
        (result.function_accessors, -1)
    in
    let free_variable_pos = last_allocated_slot + 1 in
    let free_variable_accessors, _ =
      Variable.Map.fold assign_free_variable_offset free_vars
        (result.free_variable_accessors, free_variable_pos)
    in
    let set_of_closures_id_types =
      let functions =
        Variable.Map.fold
          (fun _id (function_decl : Flambda.function_declaration) acc ->
              let arity = Flambda_utils.function_arity function_decl in
              let fields = if constant then 0 else 1 in
              { arity; fields } :: acc )
          function_decls.funs []
      in
      let fields = Variable.Map.cardinal free_vars in
      Set_of_closures_id.Map.add function_decls.set_of_closures_id
        { functions; fields } result.set_of_closures_id_types
    in
    { function_accessors; free_variable_accessors; set_of_closures_id_types }

let empty =
  { function_accessors = Closure_id.Map.empty
  ; free_variable_accessors = Var_within_closure.Map.empty
  ; set_of_closures_id_types = Set_of_closures_id.Map.empty
  }

let merge a b =
  let function_accessors =
    Closure_id.Map.disjoint_union a.function_accessors b.function_accessors
  in
  let free_variable_accessors =
    Var_within_closure.Map.disjoint_union a.free_variable_accessors
      b.free_variable_accessors
  in
  let set_of_closures_id_types =
    Set_of_closures_id.Map.disjoint_union a.set_of_closures_id_types
      b.set_of_closures_id_types
  in
  { function_accessors; free_variable_accessors; set_of_closures_id_types }

let import_for_pack ~pack_units:_ ~pack:_ _t =
  Format.eprintf "TODO wasm import_for_pack";
  empty

let compute (program : Flambda.program) =
  let state = ref empty in
  Flambda_iterators.iter_on_set_of_closures_of_program program
    ~f:(fun ~constant set_of_closures ->
      state := add_closure_offsets !state ~constant set_of_closures );
  !state
