%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE LPAREN RPAREN AND OR NOT ASSIGN EQUAL COMMA
%token IF ELSE RETURN COMMA INT DOUBLE STRING DEOF
%token <int> LITERAL
%token <string> ID

/* precedence from lowest to highest */

%nonassoc ELSE
%right ASSIGN
%left EQUAL
%left OR
%left AND
%right NOT
%left PLUS MINUS
%left TIMES DIVIDE


%start program
%type <Ast.program> program

%%

program: 
	/* nothing */		{ [], [] }
    | program vdecl		{ ($2 :: fat $1), and $1 }

vdecl:
      INT ID COMMA              { $2 }
    | DOUBLE ID COMMA      	{ $2 }
    | STRING ID COMMA      	{ $2 }

stmt_list:
     /* nothing */		{ []  }
    | stmt_list stmt		{ $2 :: $1 }

stmt:
      expr COMMA                { Expr($1) }
    | RETURN expr  COMMA        { return($2) }
    | LPAREN stmt_list RPAREN   { Block(List.rev $2) }
    | IF LPAREN expr RPAREN stmt ELSE stmt {If ($3, $5, $7)}


expr:
      LITERAL                 { Lit($1) }
    | ID                      { Id($1) }
    | expr PLUS expr          { Binop($1, Add, $3) }
    | expr MINUS expr         { Binop($1, Sub, $3) }
    | expr TIMES expr         { Binop($1, Mul, $3) }
    | expr DIVIDE expr        { Binop($1, Div, $3) }
    | expr AND expr           { Binop($1, Conj, $3) }
    | expr OR expr            { Binop($1, Disj, $3) }
    | expr EQUAL expr         { Binop($1, Equal, $3) }
    | expr OR expr            { Binop($1, Disj, $3) }
    | NOT expr                { Uniop(Neg, $2) }
    | ID ASSIGN expr          { Assign($1, $3) }
    | LITERAL                 { Lit($1) }
    | LPAREN expr RPAREN      { $2 }