{ open Parser }

rule token =
   parse [' ' '\t' '\r' '\n'] { token lexbuf }
   | '+'                      { PLUS }
   | '-'                      { MINUS }
   | '*'                      { TIMES }
   | '/'                      { DIVIDE }
   | '('                      { LPAREN }
   | ')'                      { RPAREN }
   | '='                      { ASSIGN }
   | ','                      { COMMA }
   | "puts"                   { PUTS }
   | ['0'-'9']+ as lit        { LITERAL(int_of_string lit) }
   | '"'['a'-'z' 'A'-'Z' '0'-'9']+'"' as strlit { STRLITERAL(strlit) }
   | '$'['0'-'9'] as var { VARIABLE(int_of_char var.[1] - 48) }
   | eof                      { EOF }
