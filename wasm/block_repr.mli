open Wmodule

module type Block = sig
  val make : tag:int -> Expr.t list -> Expr.t
  val make_const : tag:int -> Const.field list -> Const.t
  val get_tag : cast:bool -> Expr.t -> Expr.t
  val get_field : cast:bool -> Expr.t -> field:int -> Expr.t
  val set_field :
    cast:bool -> block:Expr.t -> Expr.t -> field:int -> Expr.no_value_expression
  val gen_block_decl : Decl.t list
  val type_decl : Type.Var.t -> int -> Decl.t list
end

module Block : Block
