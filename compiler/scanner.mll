{ open Parser }

rule token =
   parse [' ' '\t' '\r' '\n'] { token lexbuf }
   | '+'                      { PLUS }
   | '-'                      { MINUS }
   | '*'                      { TIMES }
   | '/'                      { DIVIDE }
   | '('                      { LPAREN }
   | ')'                      { RPAREN }
   | "puts"                   { PUTS }
   | ['0'-'9']+ as lit        { LITERAL(int_of_string lit) }
   | '"'['a'-'z' 'A'-'Z' '0'-'9']+'"' as strlit { STRLITERAL(strlit) }
   | eof                      { EOF }
