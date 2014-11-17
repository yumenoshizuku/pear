(*
** C Pretty Printer
** ================
** By Rui Chen, Fanxing Meng, and Risto Stevcev
**
** Adapted from the MicroC ast:
** http://www.cs.columbia.edu/~sedwards/classes/2013/w4115-summer2/microc.tar.gz  
*)

open String

(* C operator types *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type uop = Ref

type mytypes = Int | Char | Void | GtkWidget | GPointer | GChar | GList | Struct of string


(* Expressions *)
type expr =
    Literal of int
  | Id of string
  | CharLit of char
  | StrLit of string
  | ConstLit of string
  | Null
  | Binop of expr * op * expr
  | Unaryop of uop * expr
  | Assign of string * expr
  | CompoundTypeAssign of expr * expr
  | Call of string * expr list
  | Member of string * string
  | DotMember of string * string
  | OneDArrSubs of expr * expr
  | Paren of expr
  | Noexpr

(* Statements *)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type datatypeDecl =
    BasicType of mytypes
  | PointerType of mytypes 
  | PointerToPointerType of mytypes

(* Variable declaration *)
type var_decl = 
    VDecl of datatypeDecl * string
  | OneDArrDecl of datatypeDecl * string * expr

type formalDecl =
    FormalDecl of datatypeDecl * string         

(* Function declaration *)
type func_decl = {
  returnType : datatypeDecl;
  fname : string;
  formals : formalDecl list;
  locals : var_decl list;
  body : stmt list;
}

type stru_def = {
    sname : string;
    fieldDecls : var_decl list;
  } 

(* Program as a tuple of variable and function declaration lists *)
type program =  stru_def list * var_decl list * func_decl list

(* Print an expression as a string *)
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | CharLit(c) -> "'" ^ (String.make 1 c) ^ "'"
  | StrLit(s) -> "\"" ^ s ^ "\"" 
  | ConstLit(s) -> s
  | Null -> "NULL"
  | Unaryop(o, e) ->
      (match o with
	Ref -> "&" ) ^ 
      string_of_expr e  
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	     Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
       | Equal -> "==" | Neq -> "!="
       | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | CompoundTypeAssign (v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Member (structName, fieldName) -> structName ^ "->" ^ fieldName 
  | DotMember (structName, fieldName) -> structName ^ "->" ^ fieldName   
  | OneDArrSubs (arrayName, index) -> (string_of_expr arrayName) ^ "[" ^ (string_of_expr index) ^ "]"
  | Paren (e) -> "(" ^ string_of_expr e ^ ")"
  | Noexpr -> ""

(* Print a statement as a string *)
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n\t" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\t\t}\n"
  | Expr(expr) -> "\t" ^ string_of_expr expr ^ ";\n";
  | Return(expr) -> "\treturn " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "\tif (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "\tif (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n\t" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "\tfor (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "\twhile (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_datatype = function
     Int -> "int"
   | Char -> "char"
   | Void -> "void" 
   | GtkWidget -> "GtkWidget"
   | GPointer -> "gpointer"
   | GChar -> "gchar"
   | GList -> "GList"
   | Struct (name) -> "struct " ^ name

  
let rec string_of_datatypeDecl = function
     BasicType (datatype) -> (string_of_datatype datatype)
   | PointerType (datatype) -> (string_of_datatype datatype) ^ "*"  
   | PointerToPointerType (datatype) -> (string_of_datatype datatype) ^ "**"       

let rec string_of_formalDecl = function
   FormalDecl (datatypeDecl, id) -> (string_of_datatypeDecl datatypeDecl) ^ " "^ id  

(* Print the variable declarations as a string *)
let string_of_vdecl = function
     VDecl (datatypeDecl, id) -> "\t" ^ (string_of_datatypeDecl datatypeDecl) ^ " "^ id ^ ";\n"
   | OneDArrDecl (datatypeDecl, id, bound) -> 
       "\t" ^ (string_of_datatypeDecl datatypeDecl) ^ " "^ id ^ "[" ^ (string_of_expr bound)^ "]" ^ ";\n"

(* Print the function declarations as a string *)
let string_of_fdecl fdecl =
  (string_of_datatypeDecl fdecl.returnType) ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formalDecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_sdef stru_def =
  "struct " ^ stru_def.sname ^ "\n{\n" ^
  String.concat "" (List.map string_of_vdecl stru_def.fieldDecls) ^
  "};\n"

(* Print the entire program as a string *)
let string_of_program (sdefs, vars, funs) =(*
  String.concat "" (List.map string_of_sdef sdefs) ^ "\n\n" ^ *)
  String.concat "" (List.map string_of_vdecl vars) ^ "\n\n" ^
  String.concat "\n\n" (List.map string_of_fdecl funs)
