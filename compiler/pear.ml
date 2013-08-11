(*
** The Pear Translator
** ===================
** By Rui Chen, Fanxing Meng, and Risto Stevcev
**
** Website:
** https://github.com/gy3h/pear
**
** Adapted from the MicroC interpreter:
** http://www.cs.columbia.edu/~sedwards/classes/2013/w4115-summer2/microc.tar.gz
*)
(* Import modules *)
open Ast
open Cast
open Printf

(* The primitive types *)
type primitive =
    Int of int
  | String of string
  | Char of char
  | Pointer of string

(* Create a string map to store primitive types *)
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
    let rec eval env : (Ast.expr * (Cast.var_decl list * Cast.func_decl list)) ->
        (primitive * (Cast.var_decl list * Cast.func_decl list)) * (primitive NameMap.t * primitive NameMap.t) 
        = function

        (* Primitive expressions *)
	(Ast.Literal(i), cenv) -> (Int i, cenv), env
      | (Ast.StrLit(i), cenv) -> (String i, cenv), env
      | (Ast.Char(i), cenv) -> (Char i, cenv), env
      | (Ast.Pointer(i), cenv) -> (Pointer i, cenv), env

        (* Other expressions *)
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
	  let boolean i = if i then 1 else 0 in

            (* Define binary operations for the primitive expressions *)
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
            (match v with
               Int(x) ->
	         if NameMap.mem ("Int " ^ var) locals then
	          (*return Int 1? since assign succeeds?*) (Int x, cenv), (NameMap.add ("Int " ^ var) v locals, globals)
	         else if NameMap.mem ("Int " ^ var) globals then
	           (*Make decision on Int 1 vs x*) (Int x, cenv), (locals, NameMap.add ("Int " ^ var) v globals)
	         else raise (Failure ("undeclared identifier " ^ var))
             | _ -> raise (Failure ("Error: Cannot assign type")))
      | (Ast.Call("puts", [e]), cenv) ->
	  let (v, (cvars, cfuncs)), env = eval env (e, cenv) in

          (* Get main method (last function declaration) *)
          let lfdecl = List.hd (List.rev cfuncs) in

          (* Create new function declaration with same args but a new body *)
          let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; locals =
            lfdecl.locals; body = (

              (* Create the call to printf *)
              let print = 
                ( match v with

                    (* Match parameter with result *)
                    Int(x) -> 
                      Cast.Expr (Call("printf", [Cast.StrLit "\"%d\\n\""; Cast.Literal x]))
                  | String(x) -> 
                      Cast.Expr (Call("printf", [Cast.StrLit "\"%s\\n\""; Cast.StrLit ("\"" ^ x ^ "\"")]))
                  | Pointer(x) -> 
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
           ( match cfuncs with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 

          (* Interpret *)
          (match v with
             Int(x) -> print_endline (string_of_int x)
           | String(x) -> print_endline x
           | Pointer(x) -> print_endline x
           | Char(x) -> print_endline (String.make 1 x));


           (* Return the new environment *)
	   ((Int 0), (cvars, ncenv)), env
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
            (* The inner (_, globals) ignores locals, and the outer ( ,_) ignores cenv *)
	    let ((_, globals), _) = call odecl actuals globals
	    in ((Int 0), cenv), (locals, globals)
	  with ReturnException(v, globals) -> (v, cenv), (locals, globals)
    in

    (* Execute a statement and return an updated environment *)
    let rec exec (env, cenv) : Ast.stmt -> ( (primitive NameMap.t * primitive
    NameMap.t) * (Cast.var_decl list * Cast.func_decl list)) = function
	Ast.Block(stmts) -> List.fold_left exec (env, cenv) stmts
      | Ast.Expr(e) -> let (_, cenv), env = eval env (e, cenv) in (env, cenv)
      | Ast.If(e, s1, s2) ->
          let (v, cenv), env = eval env (e, cenv) in
            (match v with
              Int(x) ->
	          exec (env, cenv) (if x != 0 then s1 else s2)
            | _ -> raise (Failure ("Error: invalid operation on a conditional
            statement.")))
      | Ast.While(e, s) ->
	  let rec loop (env, cenv) =
          let (v, cenv), env = eval env (e, cenv) in
            (match v with
               Int(x) ->
	           if x != 0 then loop (exec (env, cenv) s) else env
             | _ -> raise (Failure ("Error: invalid operation on a while
             statement.")) )
          in (loop (env, cenv), cenv)
      | Ast.For(e1, e2, e3, s) ->
              let _, env = eval env (e1, cenv) in
	  let rec loop env =
          let (v, cenv), env = eval env (e2, cenv) in
            (match v with
            Int(x) ->
	    if x != 0 then
            let (_, cenv), env = eval (fst(exec (env, cenv) s)) (e3, cenv) in
	      loop (env)
	    else
	      env
            | _ -> raise (Failure ("Error: invalid operation on a for statement.")))
	  in (loop env, cenv)
      | Ast.Return(e) ->
              let (v, cenv), (locals, globals) = eval env (e, cenv) in
	  raise (ReturnException(v, globals))
      | Ast.Declare(o, v) -> 
              let (locals, globals) = env in 
              let var = ((NameMap.add v (Int 0) locals), globals) in 
              (var, cenv)

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

    (* Create the main method *)
    let new_cenv = {
        returnType = "int";
        fname = "main";
        formals = [];
        locals = [];
        body = [];
    } in

    (* Execute each statement in sequence, return updated global symbol table *)
    let env, cenv = (List.fold_left exec ((locals,globals), ([], [new_cenv]))
    odecl.obody) in
    (env, cenv)
  
  (* Run a program: initialize global variables to 0, find and run "main" *)
  in let globals = List.fold_left
      (fun globals vdecl -> NameMap.add vdecl (Int 0) globals) NameMap.empty vars
  in try
      let env, cenv = call (NameMap.find "Main" obj_decls) [] globals in
      let cvdecls, cfdecls = cenv in
      let lfdecl = List.hd (List.rev cfdecls) in
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
      ( match cfdecls with
          []  ->     []
        | [x] ->     [nfdecl]
        | x   -> x @ [nfdecl]) in  
      let listing = Cast.string_of_program (cvdecls, ncenv) in

      (* Write translation to prog.c *)
      let oc = open_out "prog.c" in 

      fprintf oc "%s\n" (* Append preprocessor args *)
                      ( "#include <stdio.h>\n" ^
                        "#include <gtk/gtk.h>\n" ^ listing ); 
  with Not_found -> raise (Failure ("did not find the main() function"))

(* Lex, parse and run program *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  run program 
