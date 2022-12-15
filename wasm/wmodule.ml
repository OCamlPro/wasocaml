module Type = Wtype
module Param = Wident.Param
module Func_id = Wident.Func_id
module Global = Wident.Global
module Expr = Wexpr

let print_list f sep ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "%s@ " sep)
    f ppf l

module Func = struct
  type t =
    | Decl of
        { params : (Param.t * Type.atom) list
        ; body : Expr.function_body
        ; type_decl : Type.Var.t option
        }
    | Import of
        { params : Type.atom list
        ; result : Type.atom list
        ; module_ : string
        ; name : string
        }

  let print ppf = function
    | Decl { params; body } ->
      let param ppf (p, typ) =
        Format.fprintf ppf "(%a: %a)" Param.print p Type.print_atom typ
      in
      let print_body ppf = function
        | Expr.Value (e, typ) ->
          Format.fprintf ppf " -> %a@ {@ %a@ }" Type.print_atom typ Expr.print e
        | Expr.No_value e ->
          Format.fprintf ppf "@ {@ %a@ }" Expr.print_no_value e
      in
      Format.fprintf ppf "@[<hov 2>Func (%a)%a@]" (print_list param ",") params
        print_body body
    | Import { params; result; module_; name } ->
      Format.fprintf ppf "@[<hov 2>Import %s %s : (%a) -> %a @]" module_ name
        (print_list Type.print_atom ",")
        params
        (print_list Type.print_atom ",")
        result
end

module Const = struct
  type field =
    | I8 of int
    | I16 of int
    | Ref_func of Func_id.t
    | Global of Global.t
    | I31 of int

  type t =
    | Struct of
        { typ : Type.Var.t
        ; fields : field list
        }
    | Expr of
        { typ : Type.atom
        ; e : Expr.t
        }
    | Import of
        { typ : Type.atom
        ; module_ : string
        ; name : string
        }

  let print_field ppf = function
    | I8 i -> Format.fprintf ppf "i8(%i)" i
    | I16 i -> Format.fprintf ppf "i16(%i)" i
    | I31 i -> Format.fprintf ppf "i31(%i)" i
    | Ref_func f -> Format.fprintf ppf "Ref_func %a" Func_id.print f
    | Global g -> Format.fprintf ppf "%a" Global.print g
end

module Decl = struct
  type const =
    { name : Global.t
    ; export : Symbol.t option
    ; descr : Const.t
    }

  type t =
    | Type of Type.Var.t * Type.descr
    | Type_rec of (Type.Var.t * Type.descr) list
    | Func of
        { name : Func_id.t
        ; descr : Func.t
        }
    | Const of const

  let print ppf = function
    | Type (var, descr) ->
      Format.fprintf ppf "type %a = %a" Type.Var.print var Type.print_descr
        descr
    | Type_rec l ->
      let pp ppf (var, descr) =
        Format.fprintf ppf "(%a = %a)" Type.Var.print var Type.print_descr descr
      in
      Format.fprintf ppf "type_rec %a" (print_list pp "") l
    | Func { name; descr } ->
      Format.fprintf ppf "@[<hov 2>func %a =@ %a@]" Func_id.print name
        Func.print descr
    | Const { name; descr = Struct { typ; fields } } ->
      Format.fprintf ppf "@[<hov 2>const %a : %a =@ {%a}@]" Global.print name
        Type.Var.print typ
        (print_list Const.print_field ";")
        fields
    | Const { name; descr = Expr { typ; e } } ->
      Format.fprintf ppf "@[<hov 2>const %a : %a =@ {%a}@]" Global.print name
        Type.print_atom typ Expr.print e
    | Const { name; descr = Import { typ; module_; name = import_name } } ->
      Format.fprintf ppf "@[<hov 2>const %a : %a =@ {%s %s}@]" Global.print name
        Type.print_atom typ module_ import_name
end

module Module = struct
  type t = Decl.t list

  let print ppf l =
    Format.fprintf ppf "@[<v 2>Module {@ %a@ }@]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
         Decl.print )
      l
end
