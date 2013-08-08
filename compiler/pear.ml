(*
** Pear Translator
** ===============
** By Rui Chen, Fanxing Meng, and Risto Stevcev
**
** Website:
** https://github.com/gy3h/pear
*)

open Ast
open Cast
open Printf

module StringMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
  end)
let vars = StringMap.empty
let objs = StringMap.empty

(* Primitive types that translate to C *)
(*
type primitive =
    Int of int
    | String of string
    | Char of char
    *)
(*
let rec eval env = function
   (Ast.Literal(x), cenv) -> (Int x, cenv), env
 | (Ast.StrLit(x), cenv) -> (String x, cenv), env
 | (Ast.Char(x), cenv) -> (Char x, cenv), env
 | (Ast.Id(x), cenv) ->
         let vars, objs = env in
         if StringMap.mem x vars then
             (StringMap.find x vars, cenv), env
         else raise (Failure ("Error: Undeclared identifier " ^ x))
         (*
 | (Ast.Seq(e1, e2), cenv) ->
         let (value, ncenv), vars = eval env (e1, cenv) in
         eval vars (e2, ncenv)*)
 | (Ast.Assign(x, e), cenv) ->
         let (value, cenv), (vars, objs) = eval env (e, cenv) in 
             (value, cenv), ((StringMap.add x value vars), objs)
 | (Ast.Call(x, e), cenv) -> (Int 1, cenv), env
 | (Ast.Puts(e1), cenv) -> 
         let (v1, (cvars, cenv)), vars = eval env (e1, cenv) in
         (* Get main method (last function declaration) *)
         let lfdecl = List.hd (List.rev cenv) in
         (* Create new function declaration with same args but a new body *)
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; locals =
             lfdecl.locals; body = (
               (* Create the call to printf *)
               let print = 
                 ( match v1 with
                     (* Match parameter with result *)
                     Int(x) -> 
                       Cast.Expr (Call("printf", [Cast.StrLit "\"%d\\n\""; Cast.Literal x]))
                   | String(x) -> 
                       Cast.Expr (Call("printf", [Cast.StrLit "\"%s\\n\""; Cast.StrLit ("\"" ^ x ^ "\"")]))
                   | Char(x) -> 
                       Cast.Expr (Call("printf", [Cast.StrLit "\"%c\\n\""; Cast.StrLit ("'" ^ (String.make 1 x) ^ "'")])) ) in
                 (* Append to the end of the body *)
                 match lfdecl.body with
                   []  ->     [print]
                 | [x] ->  x::[print]
                 | x   -> x @ [print] 
           ) } in
         (* Create new environment with the new main method *)
         let ncenv = 
           ( match cenv with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (* Return the new environment *)
         (v1, (cvars, ncenv)), env 
 | (Ast.Binop(e1, op, e2), cenv) ->
   let (v1, cenv), vars = eval env (e1, cenv) in
   let (v2, cenv), vars = eval env (e2, cenv) in
       ((match v1, v2 with
            (* Define binary operators based on types *)
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
*)
(* Create the main method *)
let cenv = {
   returnType = "int";
   fname = "main";
   formals = [];
   locals = [];
   body = [];
}
(*
let rec exec env = function
 (* Initialize environment with the main method and no variable declarations *)
   Ast.Expr(e) ->
       eval env (e, ([], [cenv]))
 | Ast.Return(e) ->
         let v, vars = eval env (e, ([], [cenv])) in
   v, vars
*)


type primitive =
    Int of int
    | String of string
    | Char of char



module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

exception ReturnException of string * string NameMap.t

(* Main entry point: run a program *)

let run (vars, funcs) =
  (* Put function declarations in a symbol table *)
  let func_decls = List.fold_left
      (fun funcs odecl -> NameMap.add odecl.oname odecl funcs)
      NameMap.empty funcs
  in

  (* Invoke a function and return an updated global symbol table *)
  let rec call odecl actuals globals =

    (* Evaluate an expression and return (value, updated environment) *)
    let rec eval env : (Ast.expr * (var_decl list * func_decl list)) ->
        (string * (var_decl list * func_decl list)) * (string NameMap.t * string NameMap.t) = function

	(Ast.Literal(i), cenv) -> (string_of_int i, cenv), env
      | (Ast.StrLit(i), cenv) -> (i, cenv), env
      | (Ast.Char(i), cenv) -> (String.make 1 i, cenv), env
      | (Ast.Noexpr, cenv) -> ("1", cenv), env (* must be non-zero for the for loop predicate *)
      | (Ast.Id(var), cenv) ->
      let locals, globals = env in
	  if NameMap.mem var locals then
	    ((NameMap.find var locals), cenv), env
	  else if NameMap.mem var globals then
	    ((NameMap.find var globals), cenv), env
	  else raise (Failure ("undeclared identifier " ^ var))
      | (Ast.Binop(e1, op, e2), cenv) ->
	  let (v1, cenv), env = eval env (e1, cenv) in
          let (v2, cenv), env = eval env (e2, cenv) in
	  let boolean i = if i then 1 else 0 in
	  (string_of_int (match op with
	    Ast.Add -> int_of_string v1 + int_of_string v2
	  | Ast.Sub -> int_of_string v1 - int_of_string v2
	  | Ast.Mul -> int_of_string v1 * int_of_string v2
	  | Ast.Div -> int_of_string v1 / int_of_string v2
	  | Ast.Equal -> boolean (v1 = v2)
	  | Ast.Neq -> boolean (v1 != v2)
	  | Ast.Less -> boolean (v1 < v2)
	  | Ast.Leq -> boolean (v1 <= v2)
	  | Ast.Greater -> boolean (v1 > v2)
	  | Ast.Geq -> boolean (v1 >= v2)), cenv), env
      | (Ast.Assign(var, e), cenv) ->
	  let (v, cenv), (locals, globals) = eval env (e, cenv) in
      (*print_endline v;*)
      
	  if NameMap.mem var locals then
	    (v, cenv), (NameMap.add var v locals, globals)
	  else if NameMap.mem var globals then
	    (v, cenv), (locals, NameMap.add var v globals)
	  else raise (Failure ("undeclared identifier " ^ var))
      | (Ast.Call("print", [e]), cenv) ->
	  let (v, cenv), env = eval env (e, cenv) in
	  print_endline v;
	  ("0", cenv), env
      | (Ast.Call(f, actuals), cenv) ->
	  let odecl =
	    try NameMap.find f func_decls
	    with Not_found -> raise (Failure ("undefined function " ^ f))
	  in
	  let actuals, env = List.fold_left
	      (fun (actuals, env) actual ->
		let (v, cenv), env = eval env (actual, cenv) in v :: actuals, env)
   	      ([], env) (List.rev actuals)
	  in
	  let (locals, globals) = env in
	  try
	    let globals = call odecl actuals globals
	    in ("0", cenv), (locals, globals)
	  with ReturnException(v, globals) -> (v, cenv), (locals, globals)
    in

    (* Execute a statement and return an updated environment *)
    let rec exec env : Ast.stmt -> string NameMap.t * string NameMap.t = function
	Ast.Block(stmts) -> List.fold_left exec env stmts
      | Ast.Expr(e) -> let (_, cenv), env = eval env (e, ([],[])) in env
      | Ast.If(e, s1, s2) ->
              let (v, cenv), env = eval env (e, ([],[])) in
	  exec env (if int_of_string v != 0 then s1 else s2)
      | Ast.While(e, s) ->
	  let rec loop env =
          let (v, cenv), env = eval env (e, ([],[])) in
	    if int_of_string v != 0 then loop (exec env s) else env
	  in loop env
      | Ast.For(e1, e2, e3, s) ->
              let (_, cenv), env = eval env (e1, ([],[])) in
	  let rec loop env =
          let (v, cenv), env = eval env (e2, ([],[])) in
	    if int_of_string v != 0 then
            let (_, cenv), env = eval (exec env s) (e3, ([],[])) in
	      loop env
	    else
	      env
	  in loop env
      | Ast.Return(e) ->
              let (v, cenv), (locals, globals) = eval env (e, ([],[])) in
	  raise (ReturnException(v, globals))
      | Ast.Declare(o, v) -> 
              let (locals, globals) = env in 
              let var = ((NameMap.add v "0" locals), globals) in 
              var

    in

    (* Enter the function: bind actual values to formal arguments *)
    let locals =
      try List.fold_left2
	  (fun locals formal actual -> NameMap.add formal actual locals)
      NameMap.empty odecl.oformals actuals
      with Invalid_argument(_) ->
	raise (Failure ("wrong number of arguments passed to " ^ odecl.oname))
    in
    (* Initialize local variables to 0 *)
    (*
    let locals = List.fold_left
	(fun locals local -> NameMap.add local "0" locals) locals odecl.locals
    in
    *)
    (* Execute each statement in sequence, return updated global symbol table *)
    snd (List.fold_left exec (locals,globals) odecl.obody)
  
  (* Run a program: initialize global variables to 0, find and run "main" *)
  in let globals = List.fold_left
      (fun globals vdecl -> NameMap.add vdecl "0" globals) NameMap.empty vars
  in try
      call (NameMap.find "main" func_decls) [] globals
  with Not_found -> raise (Failure ("did not find the main() function"))




(*
(* Print a primitive type (for debugging) *)
let string_of_primitive primitive = match primitive with
    Int(x) -> string_of_int x
 | String(x) -> x
 | Char(x) -> String.make 1 x
*)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  run program 
  (*in
  let (result, (cvars, cenv)), evars = exec (vars, objs) program in

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
  fprintf oc "%s\n" (* Append preprocessor *)
                    ( "#include <stdio.h>\n" ^
                      "#include <gtk/gtk.h>\n" ^ listing  )
*)
(*
let v1 : (string NameMap.t * string NameMap.t) -> string * (func_decl list * var_decl list) -> (string * (func_decl list * var_decl list)) * (string NameMap.t * string NameMap.t) =
    fun (nmv,nmf) (x, (y,z)) -> "a", ([],[]),(NameMap.empty v, NameMap.empty
    w)*)
