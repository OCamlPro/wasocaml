val emit : to_file:bool -> output_prefix:string -> Flambda.program -> Wat.t

val output_wat : Format.formatter -> Wat.printable_expr -> unit
