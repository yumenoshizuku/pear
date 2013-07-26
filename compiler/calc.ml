open Ast

let rec eval = function
   Lit(x) -> string_of_int x
 | StrLit(x) -> x
 | Puts(e1) -> 
         print_endline (e1);
         e1; 
 | Binop(e1, op, e2) ->
   let v1 = eval e1 and v2 = eval e2 in
       match op with
           Add -> string_of_int ((int_of_string v1) + (int_of_string v2))
         | Sub -> string_of_int ((int_of_string v1) - (int_of_string v2))
         | Mul -> string_of_int ((int_of_string v1) * (int_of_string v2))
         | Div -> string_of_int ((int_of_string v1) / (int_of_string v2))

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result = eval expr in
    print_endline (result)
