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
   Lit(x) -> string_of_int x
 | Var(x) ->
         if IntMap.is_empty vars then print_endline ("empty");
         IntMap.find x vars, env
         (*variables.(x)*)
         (*=
         if StringMap.is_empty variables then print_endline ("->" ^ x);
         if StringMap.mem x variables then 
             StringMap.find x variables
         else "100"
         =*)
 | StrLit(x) -> x
 | Seq(e1, e2) ->
         ignore (eval e1); eval e2;
 | Asn(x, e) ->
         let vars = IntMap.add x "22" vars;
         IntMap.find x vars;
         (*
         let v = eval e in
         variables.(x) <- v; v;
         *)
         (*=
         let v = eval e in
         ignore (StringMap.add x v variables); v;
         =*)
 | Puts(e1) -> 
         let v1 = eval e1 in
         print_endline (v1);
         v1; 
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
