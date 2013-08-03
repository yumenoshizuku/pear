%{ open Ast %}

%token TYPEDEF GLOBAL FUNC
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA NULL
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token REF MEMBER DOT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE 
%token INT CHAR VOID STRUCT GTKWIDGET GPOINTER GCHAR
%token <int> LITERAL
%token <string> ID 
%token <string> STRINGLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc LBRACKET
%nonassoc RBRACKET
%right ASSIGN REF 
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
  TYPEDEF sdef_list GLOBAL vdecl_list FUNC fdecl_list
  { Program( List.rev $2, List.rev $4, List.rev $6 ) }
 
 /*| program vdecl { ($2 :: fst $1), snd $1 }*/
/* | program fdecl { fst $1, ($2 :: snd $1) }*/

   /* nothing */ /*{ [], [] }*/
/* | vdecl program { ($1 :: fst $2), snd $2 } 
 | fdecl program { fst $2, ($1 :: snd $2) } 
*/    

sdef:
   STRUCT ID LBRACE vdecl_list RBRACE SEMI
   { { sname = $2;
     fieldDecls = List.rev $4 } }
     
sdef_list:
    /* nothing */    { [] }
  | sdef_list sdef { $2 :: $1 } 
 
fdecl:
   datatypeDecl ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { returnType = $1;
     fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

fdecl_list:
    /* nothing */ { [] }
  | fdecl_list fdecl { $2 :: $1 } 	 

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    datatypeDecl ID  { [FormalDecl ($1, $2)] }
  | formal_list COMMA datatypeDecl ID { FormalDecl($3, $4) :: $1 }

formalDecl: 
   datatypeDecl ID { FormalDecl ($1, $2) } 

datatypeDecl:
     INT { BasicType( Int ) }
   | INT TIMES { PointerType( Int ) }
   | CHAR { BasicType (Char) }
   | CHAR TIMES { PointerType (Char) }
   | CHAR TIMES TIMES { PointerToPointerType (Char) }
   | VOID { BasicType (Void) }
   | STRUCT ID { BasicType (Struct ($2))}
   | STRUCT ID TIMES { PointerType (Struct ($2))}   
   | GTKWIDGET TIMES { PointerType (GtkWidget ) } 
   | GTKWIDGET TIMES TIMES { PointerToPointerType (GtkWidget ) }     
   | GPOINTER { BasicType (GPointer ) } 
   | GCHAR { BasicType(GChar) }
   | GCHAR TIMES { PointerType(GChar) }   

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl: 
    datatypeDecl ID SEMI { VDecl ($1, $2) }  
  | datatypeDecl ID LBRACKET expr RBRACKET SEMI { OneDArrDecl ($1, $2, $4) }  

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | STRINGLIT        { StringLit($1) }  
  | ID               { Id($1) }
  | NULL             { Null }
  | ID MEMBER ID     { Member ($1, $3) }
  | ID DOT ID        { DotMember ($1, $3) }  
  | expr LBRACKET expr RBRACKET { OneDArrSubs($1, $3) }
  | expr PLUS expr  { Binop ($1, Add, $3) }    
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }  
  | REF expr         { Unaryop(Ref, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID MEMBER ID ASSIGN expr { CompoundTypeAssign (Member ($1, $3), $5) }  
  | ID DOT ID ASSIGN expr { CompoundTypeAssign (DotMember ($1, $3), $5) }    
  | expr LBRACKET expr RBRACKET ASSIGN expr { CompoundTypeAssign (OneDArrSubs($1, $3), $6) }    
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 } 

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
