{ open Parser }

rule token = parse 
     [' ' '\t' '\r' '\n'] { token lexbuf }
   | "(*"                 { comment lexbuf }
   | '+'      { PLUS }    | '*'      { TIMES } 
   | '-'      { MINUS }   | '/'      { DIVIDE }   
   | '('      { LPAREN }  | ')'      { RPAREN } 
   | '='      { ASSIGN }  | ','      { COMMA }    
   | "puts"   { PUTS }
   | eof      { EOF }
   | ['0'-'9']+ as lit   { LITERAL(int_of_string lit) }
   | '"'['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']+'"' as strlit  { STRLITERAL(strlit) }
   | '''['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']''' as charlit  {
       CHAR(charlit.[1]) }
   | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']+ as var { VARIABLE(var) }

and comment = parse
    "*)"      { token lexbuf }
   | _        { comment lexbuf }
