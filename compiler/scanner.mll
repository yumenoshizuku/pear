{ open Parser1 }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token =
   parse [' ' '\t' '\r' '\n'] { token lexbuf } (* Ignore whitespace *)
   | '+'                      { PLUS }
   | '-'                      { MINUS }
   | '*'                      { TIMES }
   | '/'                      { DIVIDE }
   | '('                      { LPAREN }
   | ')'                      { RPAREN }
   | "and"                    { AND}
   | "or"                     { OR }
   | "not"                    { NOT }
   | '='                      { ASSIGN }
   | "=="		      { EQUAL }
   | "if"                     { IF }
   | "else"                   { ELSE }
   | ','                      { COMMA }
   | "INT"                    { INT }
   | "DOUBLE"                 { DOUBLE }
   | "STRING"                 { STRING }
   | digit+ as lit            { Literal(int_of_string lit) }
   | letter (letter | digit | '_')* as id   { ID(id) }
   | "return"		      { RETURN }
   | eof                      { EOF }
