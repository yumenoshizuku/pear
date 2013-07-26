open Ast

module StringMap = Map.Make(String);;
let variables = StringMap.empty;;

let rec eval env = function
   Lit(x) -> string_of_int x, env
 | Var(x) ->
         if StringMap.is_empty env then print_endline ("->" ^ x);
         if StringMap.mem x env then 
             StringMap.find x env, env
         else "100", env 
             (*raise (Failure ("Error: Undeclared identifier " ^ var))*)
 | StrLit(x) -> x, env
 | Seq(e1, e2) ->
         ignore (eval env e1); eval env e2;
 | Asn(x, e) ->
         let v, env = eval env e in
         let env = StringMap.add x v env in
         StringMap.find x env, env;
 | Puts(e1) -> 
         print_endline (e1);
         e1, env; 
 | Binop(e1, op, e2) ->
   let v1, env = eval env e1 in
   let v2, env = eval env e2 in
       (match op with
           Add -> string_of_int ((int_of_string v1) + (int_of_string v2))
         | Sub -> string_of_int ((int_of_string v1) - (int_of_string v2))
         | Mul -> string_of_int ((int_of_string v1) * (int_of_string v2))
         | Div -> string_of_int ((int_of_string v1) / (int_of_string v2))), env

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result, env = eval variables expr in
    print_endline (result)
