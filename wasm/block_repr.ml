open Wstate
open Wmodule

let ref_eq = Type.Rvar Eq

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

module Block_struct : Block = struct
  let make ~tag fields : Expr.t =
    let size = List.length fields in
    State.add_block_size size;
    Struct_new
      ( Block { size }
      , I32 (Int32.of_int tag) :: I32 (Int32.of_int size) :: fields )

  let make_const ~tag fields : Const.t =
    let size = List.length fields in
    State.add_block_size size;
    let fields = [ Const.I8 tag; Const.I16 size ] @ fields in
    Struct { typ = Block { size }; fields }

  let get_tag ~(cast : bool) e : Expr.t =
    let block : Expr.t =
      match cast with false -> e | true -> Ref_cast { r = e; typ = Gen_block }
    in
    Unop (Struct_get_packed { extend = U; typ = Gen_block; field = 0 }, block)

  let get_field ~(cast : bool) e ~field : Expr.t =
    let size = field + 1 in
    State.add_block_size size;
    let typ : Type.Var.t = Block { size } in
    let e =
      match cast with false -> e | true -> Expr.(Ref_cast { typ; r = e })
    in
    Unop (Struct_get { typ; field = field + 2 }, e)

  let set_field ~(cast : bool) ~block value ~field : Expr.no_value_expression =
    let size = field + 1 in
    State.add_block_size size;
    let typ : Type.Var.t = Block { size } in
    let block =
      match cast with
      | false -> block
      | true -> Expr.(Ref_cast { typ; r = block })
    in
    NV_binop (Struct_set { typ; field = field + 2 }, (block, value))

  let gen_block_decl =
    let fields : Type.atom list = [ I8 (* tag *); I16 (* size *) ] in
    [ Decl.Type (Gen_block, Struct { sub = None; fields }) ]

  let type_descr size : Type.descr =
    let fields = List.init size (fun _ -> ref_eq) in
    let sub : Type.Var.t =
      if size <= 1 then Gen_block else Block { size = size - 1 }
    in
    Struct { sub = Some sub; fields = (* Tag *)
                                      I8 :: (* size *)
                                            I16 :: fields }

  let type_decl name size : Decl.t list = [ Decl.Type (name, type_descr size) ]
end

module Block_array : Block = struct
  let make ~tag fields : Expr.t =
    let size = List.length fields in
    State.add_block_size size;
    let tag : Expr.t = Unop (I31_new, I32 (Int32.of_int tag)) in
    Array_new_fixed { typ = Gen_block; fields = tag :: fields }

  let make_const ~tag fields : Const.t =
    let size = List.length fields in
    State.add_block_size size;
    let fields = [ Const.I31 tag ] @ fields in
    Array { typ = Gen_block; fields }

  let get_tag ~(cast : bool) e : Expr.t =
    let block : Expr.t =
      match cast with false -> e | true -> Ref_cast { r = e; typ = Gen_block }
    in
    Unop
      ( I31_get_s
      , Ref_cast { typ = I31; r = Binop (Array_get Gen_block, (block, I32 0l)) }
      )

  let get_field ~(cast : bool) e ~field : Expr.t =
    let size = field + 1 in
    State.add_block_size size;
    let e =
      match cast with
      | false -> e
      | true -> Expr.(Ref_cast { typ = Gen_block; r = e })
    in
    Binop (Array_get Gen_block, (e, I32 (Int32.of_int (field + 1))))

  let set_field ~(cast : bool) ~block value ~field : Expr.no_value_expression =
    let size = field + 1 in
    State.add_block_size size;
    let block =
      match cast with
      | false -> block
      | true -> Expr.(Ref_cast { typ = Gen_block; r = block })
    in
    Array_set
      { typ = Gen_block
      ; array = block
      ; field = I32 (Int32.of_int (field + 1))
      ; value
      }

  let gen_block_decl =
    [ Decl.Type (Gen_block, Array { sub = None; fields = ref_eq }) ]

  let type_decl _name _size : Decl.t list = []
end

let block_module =
  match block_repr with
  | Array_block -> (module Block_array : Block)
  | Struct_block -> (module Block_struct : Block)

module Block = (val block_module)
