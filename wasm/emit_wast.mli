val emit : to_file:bool -> output_prefix:string -> Flambda.program -> Wast.t

val output_wast : Format.formatter -> Wast.printable_expr -> unit
