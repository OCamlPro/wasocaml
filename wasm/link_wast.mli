
type wasm_compilation_unit = string

val link : (wasm_compilation_unit * Wast.t) list -> output:string -> unit
