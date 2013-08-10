type operator = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
     GetPty of expr * string (* expr returns a type, property name*)
    | Binop of expr * operator * expr
    | Lit of int
    | StrLit of string
    | Char of char
    | Var of string
    | Seq of expr * expr
    | Asn of string * expr
    | Puts of expr
    | Paren of expr
    | Window of string
    (* variable name, container (window) variable name, widget type and argument list *)
    | Create of string * string * string * expr list  
    | SetPty of expr * string * expr list
    (* expr(variable or getPty), action type, callback function *)
    | Action of string * string * expr 
    | Show of string
    | GtkMain
    | If of expr * expr * expr
    | IfNoElse of expr * expr
    | While of expr * expr

type stmt =
    Expr of expr
    | Return of expr   

