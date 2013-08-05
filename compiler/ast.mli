(* Operator types *)
type operator = Add | Sub | Mul | Div

(* Expression as the main type *)
type expr =
    Binop of expr * operator * expr
    | Lit of int
    | StrLit of string
    | Char of char
    | Obj of string
    | Var of string
    | Seq of expr * expr
    | Asn of string * expr
    | Puts of expr
    | Call of string * expr list
    | Declare of string * string list * expr

(* Statement that executes the program and appends the main method *)
type stmt =
    Expr of expr
    | Return of expr


(* Formals carry both their types and their values *)
type formal =
    FormalType of string
    | FormalVar of expr
