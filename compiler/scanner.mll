{ open Parser
  open String
}

rule token = parse 
     [' ' '\t' '\r' '\n'] { token lexbuf }
   | "(*"                 { comment lexbuf }
   | '+'      { PLUS }    | '*'      { TIMES } 
   | '-'      { MINUS }   | '/'      { DIVIDE }   
   | '('      { LPAREN }  | ')'      { RPAREN } 
   | '='      { ASSIGN }  | ','      { COMMA }    
   | "puts"   { PUTS }    | "return" { RETURN }
   | eof      { EOF }
   | ['0'-'9']+ as lit   { LITERAL(int_of_string lit) }
   | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']+ as obj { OBJECT(obj) }
   | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']+ as var { ID(var) }
   | '"'['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']+'"' as strlit  
       { STRLITERAL(String.sub strlit 1 (String.length strlit - 2)) }
   | '''['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']''' as charlit
       { CHAR(charlit.[1]) }

and comment = parse
    "*)"      { token lexbuf }
   | _        { comment lexbuf }
