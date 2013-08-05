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
    | Window of string
    | CreateLabel of string * string * string (* label variable name, window variable name and label initial text *) 

type stmt =
    Expr of expr
    | Return of expr

