type exception_repr =
  | Native_exceptions
  | Multi_return

type block_repr =
  | Struct_block
  | Array_block

let block_repr = Array_block
(* let block_repr = Struct_block *)

let exception_repr = Multi_return

let pp_wat = false

let unmangle_module_name = true
let uncapitalize_module_name = false

(** module names represents the wasm file path *)
let module_name_file = false

let exc_tag_module = "exn_tag"
let imports_module = "imports"
let runtime_module = "runtime"

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

module C_import_func_type = struct
  module M = struct
    type t = Wtype.Var.c_import_func_type

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

module Global_import = struct
  include Symbol
  module Set = MSet (Symbol)
end

module Func_import = struct
  module M = struct
    type t =
      { id : Closure_id.t
      ; arity : int
      }

    let compare a b = Closure_id.compare a.id b.id
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

  let c_import_func_types = ref C_import_func_type.Set.empty

  let c_imports = ref C_import.Set.empty

  let global_imports = ref Global_import.Set.empty

  let func_imports = ref Func_import.Set.empty

  let runtime_imports = ref Runtime_import.Set.empty

  let add_arity (i : Arity.t) = Arity.Set.(arities += i)

  let add_caml_apply (i : Arity.t) = Arity.Set.(caml_applies += i)

  let add_block_size i = Arity.Set.(block_sizes += i)

  let add_block_float_size i = Arity.Set.(block_float_sizes += i)

  let add_closure_type ~arity ~fields =
    add_arity arity;
    Closure_type.Set.(closure_types += { arity; fields })

  let add_c_import_func_type typ =
    C_import_func_type.Set.(c_import_func_types += typ)

  let add_c_import description = C_import.Set.(c_imports += description)

  let add_global_import description =
    Global_import.Set.(global_imports += description)

  let add_func_import description =
    add_arity description.Func_import.arity;
    Func_import.Set.(func_imports += description)

  let add_runtime_import description =
    Runtime_import.Set.(runtime_imports += description)

  let reset () =
    arities := Arity.Set.empty;
    caml_applies := Arity.Set.empty;
    block_sizes := Arity.Set.singleton 0;
    block_float_sizes := Arity.Set.singleton 0;
    closure_types := Closure_type.Set.empty;
    c_import_func_types := C_import_func_type.Set.empty;
    c_imports := C_import.Set.empty;
    global_imports := Global_import.Set.empty;
    func_imports := Func_import.Set.empty;
    runtime_imports := Runtime_import.Set.empty
end
