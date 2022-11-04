type mode =
  | Reference
  | Binarien

let mode = Binarien
(* let mode = Reference *)

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
