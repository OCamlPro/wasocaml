module Type = Wtype
module Param = Wident.Param
module Func_id = Wident.Func_id
module Global = Wident.Global
module Expr = Wexpr

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
        ; typ : Type.Var.t option
        ; module_ : string
        ; name : string
        }

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
    | Array of
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

end

module Module = struct
  type t = Decl.t list
end
