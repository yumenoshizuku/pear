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

(* Create the main method *)
let cenv = {
   returnType = "int";
   fname = "main";
   formals = [];
   locals = [];
   body = [];
}



type primitive =
    Int of int
    | String of string
    | Char of char



module NameMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
end)

exception ReturnException of primitive * primitive NameMap.t

(* Main entry point: run a program *)

let run (vars, objs) =
  (* Put function declarations in a symbol table *)
  let obj_decls = List.fold_left
      (fun objs odecl -> NameMap.add odecl.oname odecl objs)
      NameMap.empty objs
  in

  (* Invoke a function and return an updated global symbol table *)
  let rec call odecl actuals globals =

    (* Evaluate an expression and return (value, updated environment) *)
    let rec eval env : (Ast.expr * (var_decl list * obj_decl list)) ->
        (primitive * (var_decl list * obj_decl list)) * (primitive NameMap.t * primitive NameMap.t) = function

	(Ast.Literal(i), cenv) -> (Int i, cenv), env
      | (Ast.StrLit(i), cenv) -> (String i, cenv), env
      | (Ast.Char(i), cenv) -> (Char i, cenv), env
      | (Ast.Noexpr, cenv) -> (Int 1, cenv), env (* must be non-zero for the for loop predicate *)
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
          (*
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
	  let boolean i = if i then 1 else 0 in
          (match v1, v2 with
          Int(x1), Int(x2) ->
	  (Int (match op with
	    Ast.Add -> x1 + x2
	  | Ast.Sub -> x1 - x2
	  | Ast.Mul -> x1 * x2
	  | Ast.Div -> x1 / x2
	  | Ast.Equal -> boolean (x1 = x2)
	  | Ast.Neq -> boolean (x1 != x2)
	  | Ast.Less -> boolean (x1 < x2)
	  | Ast.Leq -> boolean (x1 <= x2)
	  | Ast.Greater -> boolean (x1 > x2)
	  | Ast.Geq -> boolean (x1 >= x2)), cenv)
          | _ -> raise (Failure("Error: invalid operation on a binary expression."))), env
      | (Ast.Assign(var, e), cenv) ->
	  let (v, cenv), (locals, globals) = eval env (e, cenv) in
      (*print_endline var;*)
       (match v with
       Int(x) ->
	  if NameMap.mem var locals then
	    (Int x, cenv), (NameMap.add var v locals, globals)
	  else if NameMap.mem var globals then
	    (Int x, cenv), (locals, NameMap.add var v globals)
	  else raise (Failure ("undeclared identifier " ^ var))
      | _ -> raise (Failure ("Error: Cannot assign type")))
      | (Ast.Call("print", [e]), cenv) ->
	  let (v, cenv), env = eval env (e, cenv) in
          (match v with
            Int(x) -> print_endline (string_of_int x)
          | String(x) -> print_endline x
          | Char(x) -> print_endline (String.make 1 x));
	  ((Int 0), cenv), env
      | (Ast.Call(f, actuals), cenv) ->
	  let odecl =
	    try NameMap.find f obj_decls
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
	    in ((Int 0), cenv), (locals, globals)
	  with ReturnException(v, globals) -> (v, cenv), (locals, globals)
    in

    (* Execute a statement and return an updated environment *)
    let rec exec env : Ast.stmt -> primitive NameMap.t * primitive NameMap.t = function
	Ast.Block(stmts) -> List.fold_left exec env stmts
      | Ast.Expr(e) -> let (_, cenv), env = eval env (e, ([],[])) in env
      | Ast.If(e, s1, s2) ->
          let (v, cenv), env = eval env (e, ([],[])) in
            (match v with
              Int(x) ->
	          exec env (if x != 0 then s1 else s2)
            | _ -> raise (Failure ("Error: invalid operation on a conditional
            statement.")))
      | Ast.While(e, s) ->
	  let rec loop env =
          let (v, cenv), env = eval env (e, ([],[])) in
            (match v with
               Int(x) ->
	           if x != 0 then loop (exec env s) else env
             | _ -> raise (Failure ("Error: invalid operation on a while
             statement.")) )
          in loop env
      | Ast.For(e1, e2, e3, s) ->
              let (_, cenv), env = eval env (e1, ([],[])) in
	  let rec loop env =
          let (v, cenv), env = eval env (e2, ([],[])) in
            (match v with
            Int(x) ->
	    if x != 0 then
            let (_, cenv), env = eval (exec env s) (e3, ([],[])) in
	      loop env
	    else
	      env
            | _ -> raise (Failure ("Error: invalid operation on a for statement.")))
	  in loop env
      | Ast.Return(e) ->
              let (v, cenv), (locals, globals) = eval env (e, ([],[])) in
	  raise (ReturnException(v, globals))
      | Ast.Declare(o, v) -> 
              (*fix*)
              let (locals, globals) = env in 
              
              let var = ((NameMap.add v (Int 0) locals), globals) in 
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
    let locals = List.fold_left
	(fun locals local -> NameMap.add local (Int 0) locals) locals odecl.olocals
    in
    (* Execute each statement in sequence, return updated global symbol table *)
    snd (List.fold_left exec (locals,globals) odecl.obody)
  
  (* Run a program: initialize global variables to 0, find and run "main" *)
  in let globals = List.fold_left
      (fun globals vdecl -> NameMap.add vdecl (Int 0) globals) NameMap.empty vars
  in try
      call (NameMap.find "main" obj_decls) [] globals
  with Not_found -> raise (Failure ("did not find the main() function"))


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
(*
let rec exec env = function
 (* Initialize environment with the main method and no variable declarations *)
   Ast.Expr(e) ->
       eval env (e, ([], [cenv]))
 | Ast.Return(e) ->
         let v, vars = eval env (e, ([], [cenv])) in
   v, vars
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
let v1 : (string NameMap.t * string NameMap.t) -> string * (obj_decl list * var_decl list) -> (string * (obj_decl list * var_decl list)) * (string NameMap.t * string NameMap.t) =
    fun (nmv,nmf) (x, (y,z)) -> "a", ([],[]),(NameMap.empty v, NameMap.empty
    w)*)
