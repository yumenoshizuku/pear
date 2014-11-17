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
   | "Window" { WINDOW }
   | "Fixed" | "Frame" | "Grid" | "Vbox"
   | "Menubar" | "Menu" | "Menuitem" as wid  { CREATE(wid) }
   | '.'      { DOT }
   | "Title" | "WinSize" | "WinPosition" | "WinMove"
   | "Contain" | "FixPos" | "BorderWidth" | "Resizable" | "BoxPack"
   | "FrameLabel" | "FrameShadow" | "GridAttach" | "InsertRow" | "InsertColumn"
   | "MenuItemLabel" | "InMenu" | "AppendMenu" | "ClickQuit" as pty { SETPTY(pty) }
   | ['0'-'9']+ as lit   { LITERAL(int_of_string lit) }
   | '"' ['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n']+ '"' as strlit  {
       STRLITERAL(String.sub strlit 1 (String.length strlit - 2)) }
   | ''' ['a'-'z' 'A'-'Z' '0'-'9' ' ' '\t' '\r' '\n'] ''' as charlit  {
       CHAR(charlit.[1]) }
   | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']+ as var { VARIABLE(var) }

and comment = parse
    "*)"      { token lexbuf }
   | _        { comment lexbuf }
