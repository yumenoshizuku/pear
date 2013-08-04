%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF PUTS COMMA ASSIGN
%token <char> CHAR
%token <int> LITERAL
%token <string> STRLITERAL
%token <string> ID
%token <string> OBJECT
%token LPAREN RPAREN
%token RETURN

%left COMMA
%left ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE
%left PUTS

%start stmt
%type <Ast.expr> expr
%type <Ast.stmt> stmt

%%

expr:
    expr PLUS expr          { Binop($1, Add, $3) }
  | expr MINUS expr         { Binop($1, Sub, $3) }
  | expr TIMES expr         { Binop($1, Mul, $3) }
  | expr DIVIDE expr        { Binop($1, Div, $3) }
  | PUTS LPAREN expr RPAREN { Puts($3) }
  | LITERAL                 { Lit($1) }
  | STRLITERAL              { StrLit($1) }
  | CHAR                    { Char($1) }
  | ID                      { Var($1) }
  | ID ASSIGN expr          { Asn($1, $3) }
  | expr COMMA expr         { Seq($1, $3) }
  | LPAREN expr RPAREN      { $2 }
  | OBJECT LPAREN actuals_opt RPAREN { Call($1, $3) }
  | OBJECT LPAREN formals_opt RPAREN LPAREN expr RPAREN { Declare($1, $3, $6) } 

stmt:
    expr { Expr($1) }
  | RETURN expr { Return($2) }

/* An optional list of formal arguments during object declaration */
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

/* An optional list of actual arguments during object calls */
actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
