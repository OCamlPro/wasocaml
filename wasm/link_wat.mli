type wasm_compilation_unit = string

val link : (wasm_compilation_unit * Wat.t) list -> output:string -> unit
