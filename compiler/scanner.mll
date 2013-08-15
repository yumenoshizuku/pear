{ 
  open Parser
  open String
}

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "(*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACE }
| ']'      { RBRACE }

| ','      { COMMA }
| '.'      { DOT }

| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }

| '='      { ASSIGN }

| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }

| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "def"    { DEFINE }

| ['0'-'9']+ as lxm        { LITERAL(int_of_string lxm) }
| ['0'-'9']*'.'['0'-'9']+ as lxm { FLOLIT(float_of_string lxm) }
| '"'[^'"']+'"' as lxm     { STRLIT(String.sub lxm 1 (String.length lxm - 2)) }
| '''[^''']''' as charlit  { CHAR(charlit.[1]) }
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as obj { OBJECT(obj) } 

| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*)" { token lexbuf }
| _    { comment lexbuf }
