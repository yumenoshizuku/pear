open Ast

(*=
module StringMap = Map.Make(String);;
let variables = StringMap.empty;;
=*)
module IntMap = Map.Make(struct
  type t = int
  let compare x y = Pervasives.compare x y
  end)
let vars = IntMap.empty;;
(*let variables = Array.make 10 "";;*)

let rec eval env = function
   Lit(x) -> string_of_int x, env
 | Var(x) ->
         (*if IntMap.is_empty vars then print_endline ("empty");*)
         if IntMap.mem x env then
             IntMap.find x env, env
         else raise (Failure ("undeclared identifier" ^ string_of_int x))
         (*variables.(x)*)
         (*=
         if StringMap.is_empty variables then print_endline ("->" ^ x);
         if StringMap.mem x variables then 
             StringMap.find x variables
         else "100"
         =*)
 | StrLit(x) -> x, env
 | Seq(e1, e2) ->
         let value, vars = eval env e1 in
         eval vars e2;
 | Asn(x, e) ->
         let value, vars = eval env e in 
             value, (IntMap.add x value vars);
         (*
         let v = eval e in
         variables.(x) <- v; v;
         *)
         (*=
         let v = eval e in
         ignore (StringMap.add x v variables); v;
         =*)
 | Puts(e1) -> 
         let v1, vars = eval env e1 in
         print_endline (v1);
         v1, env; 
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
    let result, env = eval vars expr in
    print_endline (result)
