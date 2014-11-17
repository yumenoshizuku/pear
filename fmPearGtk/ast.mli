type operator = Add | Sub | Mul | Div

type expr =
      Binop of expr * operator * expr
    | Lit of int
    | StrLit of string
    | Char of char
    | Var of string
    | Seq of expr * expr
    | Asn of string * expr
    | Puts of expr
    | Create of string * string
    | Window of string
    | Set of string * string * expr list


type stmt =
      Expr of expr
    | Return of expr

