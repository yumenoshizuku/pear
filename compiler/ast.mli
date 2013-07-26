type operator = Add | Sub | Mul | Div

type expr =
    Binop of expr * operator * expr
    | Lit of int
    | StrLit of string
    (*| Var of string*)
    | Var of int
    | Seq of expr * expr
    | Asn of int * expr
    | Puts of expr
