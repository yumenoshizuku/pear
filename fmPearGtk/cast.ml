type mytypes = Int | Char | Void | GtkWidget | GPointer | GChar | Struct of string
type uop = Ref
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    Literal of int
  | Id of string
  | StrLit of string  
  | Null
  | Unaryop of uop * expr
  | Binop of expr * op * expr
  | Assign of string * expr
  | CompoundTypeAssign of expr * expr
  | Call of string * expr list
  | Member of string * string
  | DotMember of string * string
  | OneDArrSubs of expr * expr
  | Noexpr

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
  
type vdecl =
    VDecl of datatypeDecl * string
  | OneDArrDecl of datatypeDecl * string * expr
    
type formalDecl =
    FormalDecl of datatypeDecl * string         

type func_decl = {
    returnType : datatypeDecl;
    fname : string;
    formals : formalDecl list;
    locals : vdecl list;
    body : stmt list;
  }

type sdef = {
    sname : string;
    fieldDecls : vdecl list;
  } 

type program = 
   Program of sdef list * vdecl list * func_decl list
(*{
   vdecls : vdecl list;
   fdecls : func_decl list;
}
*)
(*vdecl list * func_decl list *)






let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | StrLit(s) -> s 
  | Null -> "NULL" 
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" ) ^ " " ^
      string_of_expr e2
  | Unaryop(o, e) ->
      (match o with
	Ref -> "&" ) ^ 
      string_of_expr e      
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | CompoundTypeAssign (v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Member (structName, fieldName) -> structName ^ "->" ^ fieldName 
  | DotMember (structName, fieldName) -> structName ^ "->" ^ fieldName   
  | OneDArrSubs (arrayName, index) -> (string_of_expr arrayName) ^ "[" ^ (string_of_expr index) ^ "]"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_datatype =function
     Int -> "int"
   | Char -> "char"
   | Void -> "void" 
   | GtkWidget -> "GtkWidget"
   | GPointer -> "gpointer"
   | GChar -> "gchar"
   | Struct (name) -> "struct " ^ name
   
let rec string_of_datatypeDecl = function
     BasicType (datatype) -> (string_of_datatype datatype)
   | PointerType (datatype) -> (string_of_datatype datatype) ^ "*"  
   | PointerToPointerType (datatype) -> (string_of_datatype datatype) ^ "**"       

let rec string_of_vdecl = function
     VDecl (datatypeDecl, id) -> (string_of_datatypeDecl datatypeDecl) ^ " "^ id ^ ";\n"
   | OneDArrDecl (datatypeDecl, id, bound) -> 
        (string_of_datatypeDecl datatypeDecl) ^ " "^ id ^ "[" ^ (string_of_expr bound)^ "]" ^ ";\n"
   
let rec string_of_formalDecl = function
   FormalDecl (datatypeDecl, id) -> (string_of_datatypeDecl datatypeDecl) ^ " "^ id  

let string_of_fdecl fdecl =
  (string_of_datatypeDecl fdecl.returnType) ^ " " ^ 
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formalDecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"


let string_of_sdef sdef =
  "struct " ^ sdef.sname ^ "\n{\n" ^
  String.concat "" (List.map string_of_vdecl sdef.fieldDecls) ^
  "};\n"


let rec string_of_program = function
  Program (sdefs, vars, funs) ->
  String.concat "" (List.map string_of_sdef sdefs) ^ "\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funs)

(*
let string_of_program (vars, funs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funs)
*)
(*
let string_of_program program =
  String.concat "" (List.map string_of_vdecl program.vdecls) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl program.fdecls)
  (* (string_of_fdecl program.fdecls) *)
  *)
