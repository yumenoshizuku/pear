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
    (* variable name, container (window) variable name, widget type and argument list *)
    | Create of string * string * string * expr list  
    | GetPty of string * string (* var name, property name*)
    | SetPty of string * string * expr list
    (* expr(variable or getPty), action type, callback function *)
    | Action of string * string * expr 

type stmt =
    Expr of expr
    | Return of expr   

