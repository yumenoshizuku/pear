%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF PUTS COMMA ASSIGN DOT SEMI
%token <int> LITERAL
%token <string> STRLITERAL
%token <char> CHAR
%token <string> VARIABLE
%token <string> GETPTY
%token <string> SETPTY
%token <string> CREATE
%token <string> ACTION
%token SHOW
%token WINDOW 
%token GTKMAIN
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
    | expr COMMA expr         { Seq($1, $3) }
    | PUTS LPAREN expr RPAREN { Puts($3) }
    | LITERAL                 { Lit($1) }
    | STRLITERAL              { StrLit($1) }
    | CHAR                    { Char($1) }
    | VARIABLE                { Var($1) }
    | VARIABLE ASSIGN expr    { Asn($1, $3) }
    /*| expr COMMA expr         { Seq($1, $3) }*/
    | LPAREN expr RPAREN      { $2 }
    | VARIABLE ASSIGN WINDOW LPAREN RPAREN   { Window($1) }
    | VARIABLE ASSIGN VARIABLE DOT CREATE LPAREN actuals_opt RPAREN { Create ($1, $3, $5, $7) }    
    | VARIABLE DOT GETPTY LPAREN RPAREN { GetPty($1, $3) }
    | VARIABLE DOT SETPTY LPAREN actuals_opt RPAREN { SetPty($1, $3, $5) }
    | VARIABLE DOT ACTION LPAREN expr RPAREN { Action($1, $3, $5) }
    | VARIABLE DOT SHOW LPAREN RPAREN { Show ($1) }
    | GTKMAIN { GtkMain }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list SEMI expr { $3 :: $1 }    

stmt:
      expr { Expr($1) }
    | RETURN expr { Return($2) }     
