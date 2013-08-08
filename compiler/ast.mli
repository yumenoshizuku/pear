(* Operator types *)
type op = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater | Geq

(* Expression as the main type *)
type expr =
    Literal of int
  | StrLit of string
  | Char of char
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

(* Statement that executes the program and appends the main method *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Declare of string * string


(* Variable declaration *)
  (*
type var_decl = {
  varType : string;
  vname : string;
}
*)

(* Function declaration *)
type obj_decl = {
  oname : string;
  oformals : string list;
  olocals : string list;
  obody : stmt list;
}

type program = string list * obj_decl list

(* Formals carry both their types and their values *)
(*
type formal =
    FormalType of string
    | FormalVar of expr
    *)
