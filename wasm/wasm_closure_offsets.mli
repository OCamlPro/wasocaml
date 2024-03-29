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

(** Assign numerical offsets, within closure blocks, for code pointers and
    environment entries. *)

type function_accessor = private
  { field : int
  ; recursive_set : bool
  ; arity : int
  ; set : Set_of_closures_id.t
  }

type free_var_accessor = private
  { field : int
  ; recursive_set : bool
  ; closure_size : int
  ; set : Set_of_closures_id.t
  }

type func = private
  { arity : int
  ; fields : int
  }

type set_of_closures_id_type = private
  { functions : func list
  ; fields : int
  }

type t = private
  { function_accessors : function_accessor Closure_id.Map.t
  ; free_variable_accessors : free_var_accessor Var_within_closure.Map.t
  ; set_of_closures_id_types : set_of_closures_id_type Set_of_closures_id.Map.t
  }

val empty : t

val merge : t -> t -> t

val import_for_pack :
  pack_units:Compilation_unit.Set.t -> pack:Compilation_unit.t -> t -> t

val compute : Flambda.program -> t
