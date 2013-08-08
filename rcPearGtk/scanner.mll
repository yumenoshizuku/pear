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
   | '.'      { DOT }
   | "puts"   { PUTS }    | "return" { RETURN }
   | eof      { EOF }
   | ';'      { SEMI }
   | "Window" { WINDOW }
   | "getText" as lit { GETPTY (lit) }
   | "setText" as lit { SETPTY (lit) }
   | "Label" | "Button" as lit { CREATE (lit) }
   | "click" as lit { ACTION (lit) }
   | ['0'-'9']+ as lit   { LITERAL(int_of_string lit) }
   | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as var { VARIABLE(var) }
   | '"'['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']*'"' as strlit  
       { STRLITERAL(String.sub strlit 1 (String.length strlit - 2)) }
   | '''['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']''' as charlit
       { CHAR(charlit.[1]) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }       

and comment = parse
    "*)"      { token lexbuf }
   | _        { comment lexbuf }
