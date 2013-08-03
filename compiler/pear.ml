open Ast
open Cast
open Printf

module StringMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
  end)
let vars = StringMap.empty

type primitive =
    Int of int
    | String of string
    | Char of char


let rec eval env = function
   (Ast.Lit(x), cenv) -> (Int x, cenv), env
 | (Ast.StrLit(x), cenv) -> (String x, cenv), env
 | (Ast.Char(x), cenv) -> (Char x, cenv), env
 | (Ast.Var(x), cenv) ->
         if StringMap.mem x env then
             (StringMap.find x env, cenv), env
         else raise (Failure ("Error: Undeclared identifier " ^ x))
 | (Ast.Seq(e1, e2), cenv) ->
         (*let (value, cenv2), vars = eval env (e1, cenv) in*)
         let (value, ncenv), vars = eval vars (e1, cenv) in
         eval vars (e2, ncenv)
 | (Ast.Asn(x, e), cenv) ->
         let (value, cenv), vars = eval env (e, cenv) in 
             (value, cenv), (StringMap.add x value vars)
 | (Ast.Puts(e1), cenv) -> 
         let (v1, (cvars, cenv)), vars = eval env (e1, cenv) in
         let lfdecl = List.hd (List.rev cenv) in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; locals =
             lfdecl.locals; body = (
               let print = 
                 ( match v1 with
                     Int(x) -> 
                       Cast.Expr (Call("printf", [Cast.StrLit "\"%d\\n\""; Cast.Literal x]))
                   | String(x) -> 
                       Cast.Expr (Call("printf", [Cast.StrLit "\"%s\\n\""; Cast.StrLit ("\"" ^ x ^ "\"")]))
                   | Char(x) -> 
                       Cast.Expr (Call("printf", [Cast.StrLit "\"%c\\n\""; Cast.StrLit ("'" ^ (String.make 1 x) ^ "'")])) ) in
                 match lfdecl.body with
                   []  ->     [print]
                 | [x] ->  x::[print]
                 | x   -> x @ [print] 
           ) } in
         let ncenv = 
           ( match cenv with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (v1, (cvars, ncenv)), env 
 | (Ast.Binop(e1, op, e2), cenv) ->
   let (v1, cenv), vars = eval env (e1, cenv) in
   let (v2, cenv), vars = eval env (e2, cenv) in
       ((match v1, v2 with
            Int(x1), Int(x2) ->
            Int (match op with
                   Ast.Add -> x1 + x2
                 | Ast.Sub -> x1 - x2
                 | Ast.Mul -> x1 * x2
                 | Ast.Div -> x1 / x2)
         |  String(x1), String(x2) ->
            String (match op with
                   Ast.Add -> x1 ^ x2
                 | _ -> raise (Failure 
                     ("Error: Invalid string operation.")))
         | _ -> raise (Failure 
                     ("Error: Invalid operation.")) 
        ), cenv), vars

let cenv = {
   returnType = "int";
   fname = "main";
   formals = [];
   locals = [];
   body = [];
}

let rec exec env = function
   Ast.Expr(e) ->
       eval env (e, ([], [cenv]))
 | Ast.Return(e) ->
         let v, vars = eval env (e, ([], [cenv])) in
   v, vars
   
type action = SwAst | SwCast (*| SwInterpret*)

let string_of_primitive primitive = match primitive with
    Int(x) -> string_of_int x
 | String(x) -> x
 | Char(x) -> String.make 1 x

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.stmt Scanner.token lexbuf in
  let (result, (cvars, cenv)), evars = exec vars program in

  (* Append "return 0;" at the end of the main method *) 
  let lfdecl = List.hd (List.rev cenv) in
  let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; locals =
    lfdecl.locals; body = (
      (* Append "return 0;" as the last body declaration *)
      let main_return = Cast.Return(Cast.Literal 0) in
      match lfdecl.body with
        []  ->     [main_return]
      | [x] ->  x::[main_return]
      | x   -> x @ [main_return] 
      ) } in
  (* Append to the new cenv *)
  let ncenv = 
    ( match cenv with
        []  ->     []
      | [x] ->     [nfdecl]
      | x   -> x @ [nfdecl]) in  
  let listing = Cast.string_of_program (cvars, ncenv) in
  let oc = open_out "prog.c" in 
  fprintf oc "%s\n" listing
