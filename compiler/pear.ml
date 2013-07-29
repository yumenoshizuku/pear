open Ast
open Printf

module StringMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
  end)
let vars = StringMap.empty;;

let rec eval env = function
   Lit(x) -> string_of_int x, env
 | Var(x) ->
         if StringMap.mem x env then
             StringMap.find x env, env
         else raise (Failure ("Error: Undeclared identifier " ^ x))
 | StrLit(x) -> x, env
 | Seq(e1, e2) ->
         let value, vars = eval env e1 in
         eval vars e2;
 | Asn(x, e) ->
         let value, vars = eval env e in 
             value, (StringMap.add x value vars);
 | Puts(e1) -> 
         let v1, vars = eval env e1 in
         (* Printf for puts and char* for string *)
         ("printf(\"%s\\n\", " ^ v1 ^ ");"), env; 
 | Binop(e1, op, e2) ->
   let v1, vars = eval env e1 in
   let v2, vars = eval env e2 in
       (match op with
           Add -> string_of_int ((int_of_string v1) + (int_of_string v2))
         | Sub -> string_of_int ((int_of_string v1) - (int_of_string v2))
         | Mul -> string_of_int ((int_of_string v1) * (int_of_string v2))
         | Div -> string_of_int ((int_of_string v1) / (int_of_string v2))), vars

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result, evars = eval vars expr in
    let oc = open_out "prog.c" in
    (* Wrap main method and libraries *)
    fprintf oc "%s\n" ("#include <stdio.h>\n#include <gtk/gtk.h>\nint main() {\n" ^ result ^ "\n}")

