%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF PUTS COMMA ASSIGN
%token <int> LITERAL
%token <string> STRLITERAL
%token <char> CHAR
%token <string> VARIABLE
%token LPAREN RPAREN

%left COMMA
%left ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE
%left PUTS

%start expr
%type <Ast.expr> expr

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
    | VARIABLE                { Var($1) }
    | VARIABLE ASSIGN expr    { Asn($1, $3) }
    | expr COMMA expr         { Seq($1, $3) }
    | LPAREN expr RPAREN      { $2 }
