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
    let rec eval env : (Ast.expr * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list)) ->
        (primitive * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list)) * (primitive NameMap.t * primitive NameMap.t) 
        = function

        (* Primitive expressions *)
	(Ast.Literal(i), cenv) -> (Int i, cenv), env
      | (Ast.StrLit(i), cenv) -> (String i, cenv), env
      | (Ast.CharLit(i), cenv) -> (Char i, cenv), env

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
	         if NameMap.mem var locals then
	           (Int x, cenv), (NameMap.add var v locals, globals)
	         else if NameMap.mem var globals then
	           (Int x, cenv), (locals, NameMap.add var v globals)
	         else raise (Failure ("undeclared identifier " ^ var))
             | _ -> raise (Failure ("Error: Cannot assign type")))

      | (Ast.Call("puts", [e]), cenv) ->
	  let (v, (cstrs, cvars, cfuncs)), env = eval env (e, cenv) in

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
                      Cast.Expr (Call("printf", [StrLit "%d\\n"; Literal x]))
                  | String(x) -> 
                      Cast.Expr (Call("printf", [StrLit "%s\\n"; StrLit x]))
                  | Char(x) -> 
                      Cast.Expr (Call("printf", [StrLit "%c\\n"; CharLit x])) ) in
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
           | Char(x) -> print_endline (String.make 1 x));

           (* Return the new environment *)
	   ((Int 0), (cstrs, cvars, ncenv)), env
      | (Ast.Call(f, actuals), cenv) ->
		let (st, gl, fs) = cenv in
		let (locals, globals) = env in
  		if not ((NameMap.mem f locals) or (NameMap.mem f globals)) then
 		 raise (Failure ("Error: Unknown function " ^ f))
  		else match (List.hd actuals, List.hd (List.tl actuals)) with
			(Ast.Id(w), Ast.StrLit(s)) ->
			if ((NameMap.mem w locals) or (NameMap.mem w globals)) then ( let nfdecl = {
		returnType = BasicType(Cast.Void);
        fname = f;
        formals = [FormalDecl (PointerType (GtkWidget), "widget");
   		   FormalDecl (BasicType (GPointer), w)];
        locals = [];
        body = [
		match ((if NameMap.mem f locals then NameMap.find f locals else NameMap.find f globals), (if NameMap.mem w locals then NameMap.find w locals else NameMap.find w globals), Ast.StrLit(s)) with
		  (String("Display"), String("Label"), Ast.StrLit(s)) ->
	(Cast.Expr (Call("gtk_label_set_text ", [Call("GTK_LABEL",[Id w]); StrLit s])))
		
		| _ -> raise (Failure ("Error: Callback function not supported."))
		];}
		in let ncenv = 
           ( match fs with
               []  ->     []
             | [x] -> [nfdecl] @ [x]
             | x   -> [nfdecl] @ x)
	in (Int 0, (st, gl, ncenv))), env

	 else raise (Failure ("Error: Unknown widget " ^ w))

		| _ -> let odecl =
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
    NameMap.t) * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list)) = function
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
      | Ast.Set(id, obj, args) -> 
	let (s, g, f) = cenv in
              let (plocals, pglobals) = env in 
  		if not (NameMap.mem id plocals or NameMap.mem id pglobals) then
 	(
		let var = ((NameMap.add id (String obj) plocals), pglobals) in 
		let lfdecl = List.hd (List.rev f) in
		let nfdecl = {  returnType = lfdecl.returnType; 
				fname = lfdecl.fname; 
				formals = lfdecl.formals; 
             			locals= (
		if (obj = "Display") then lfdecl.locals else (
               let print =
                 (Cast.VDecl (Cast.PointerType (Cast.GtkWidget), id))   
				 in
                 match lfdecl.locals with
                   []  ->     [print]
                 | [x] ->  x::[print]
                 | x   -> x @ [print] 
           )) ; body= (
               let print = 
		(match (obj, args) with
		  ("Window", []) -> 
	[Cast.Expr (Assign (id, (Call ("gtk_window_new", [ConstLit ("GTK_WINDOW_TOPLEVEL")])))); 
	Cast.Expr(Call("g_signal_connect",[Id id; StrLit "destroy" ; Call("G_CALLBACK",[ConstLit ("gtk_main_quit")]); Null]))]
		| ("Fixed", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_fixed_new", []))))]
		| ("Frame", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_frame_new", [Null]))))]
		| ("Grid", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_grid_new", []))))]
		| ("Vbox", [Ast.Id "hom"]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("TRUE")]))]
		| ("Vboxhom", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("TRUE")]))]
		| ("Vbox", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("FALSE")]))]
		| ("Box", [Ast.Id "vertical"]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("FALSE")]))]
		| ("Box", [Ast.Id "horizontal"]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_HORIZONTAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("FALSE")]))]
		| ("Hbox", [Ast.Id "hom"]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_HORIZONTAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("TRUE")]))]
		| ("Hboxhom", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_HORIZONTAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("TRUE")]))]
		| ("Hbox", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_HORIZONTAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("FALSE")]))]
        | ("Button", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_button_new", []))))]
		| ("Button", [Ast.StrLit s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_button_new_with_label", [StrLit s]))))]
		| ("Label", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_label_new ", [Null]))))]
		| ("Checkbox", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_check_button_new", []))))]	
		| ("Checkbox", [Ast.StrLit s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_check_button_new_with_label", [StrLit s]))))]	
		| ("Menubar", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_bar_new", []))))]
		| ("Menu", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_new", []))))]
		| ("Menuitem", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_item_new", []))))]
		| ("Menuitem", [Ast.StrLit s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_item_new_with_label", [StrLit s]))))]
		| ("Display", []) -> []
		| _  -> raise (Failure ("Error: Object not supported."))) in
                 match lfdecl.body with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print 
           )} in
         let ncenv = 
           ( match f with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> List.rev([nfdecl]@(List.tl (List.rev x)))) in 
              (var, (s, g, ncenv))
)
  		else(
         let lfdecl = List.hd (List.rev f) in
		 let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals; body= (
               let print = 
		match ((if NameMap.mem id plocals then NameMap.find id plocals else NameMap.find id pglobals), obj, args) with
		  (String("Window"), "Title", [Ast.StrLit s]) ->
	[Cast.Expr (Call("gtk_window_set_title",
	[Call("GTK_WINDOW",[Id id]); StrLit s]))]
		| (String("Window"), "Position", [Ast.Id "center"]) ->
	[Cast.Expr (Call("gtk_window_set_position",
	[Call("GTK_WINDOW",[Id id]); ConstLit("GTK_WIN_POS_CENTER")]))]
		| (String("Window"), "Move", [Ast.Literal x; Ast.Literal y]) ->
	[Cast.Expr (Call("gtk_window_move",
	[Call("GTK_WINDOW",[Id id]); Literal x; Literal y]))]
		| (String("Window"), "Size", [Ast.Literal w; Ast.Literal h]) ->
	[Cast.Expr (Call("gtk_window_set_default_size",
	[Call("GTK_WINDOW",[Id id]); Literal w; Literal h]))]
		| (_, "Size", [Ast.Literal w; Ast.Literal h]) ->
	[Cast.Expr (Call("gtk_widget_set_size_request",
	[Call("GTK_WINDOW",[Id id]); Literal w; Literal h]))]
		| (String("Window"), "Resizable", [Ast.Id("no")]) ->
	[Cast.Expr (Call("gtk_window_set_resizable",
	[Call("GTK_WINDOW",[Id id]); ConstLit("TRUE")]))]
		| (String("Frame"), "Label", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_frame_set_label",
	[Cast.Call("GTK_FRAME",[Cast.Id id]); StrLit s])))]
		| (String("Button"), "Label", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_button_set_label",
	[Cast.Call("GTK_BUTTON",[Cast.Id id]); StrLit s])))]
		| (String("Label"), "Text", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_label_set_text ",
	[Cast.Call("GTK_LABEL",[Cast.Id id]); StrLit s])))]
		| (String("Frame"), "Shadow", [Ast.StrLit s]) ->
		(match s with
		  	"in" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_IN")]))]
			| "out" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_OUT")]))]
			| "etchedin" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_ETCHED_IN")]))]
			| "etchedout" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_ETCHED_OUT")]))]
			| _ -> raise (Failure ("Error: Shadow " ^ s ^ " is invalid.")))
		| (String("Grid"), "InsertRowAt", [Ast.Literal r]) ->
	[Cast.Expr (Call("gtk_grid_insert_row",
	[Call("GTK_GRID",[Id id]); Literal r]))]
		| (String("Grid"), "InsertColumnAt", [Ast.Literal c]) ->
	[Cast.Expr (Call("gtk_grid_insert_column",
	[Call("GTK_GRID",[Id id]); Literal c]))]
		| (String("Grid"), "AttachAt", [Ast.Id wid; Ast.Literal x; Ast.Literal y; Ast.Literal c; Ast.Literal r]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_grid_attach",
	[Call("GTK_GRID",[Id id]); Id wid; Literal x; Literal y; Literal c; Literal r]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "Contain", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_container_add",
	[Call("GTK_CONTAINER",[Id id]); Id wid]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "FixPos", [Ast.Id wid; Ast.Literal x; Ast.Literal y]) ->
		if (NameMap.find wid plocals = String("Fixed")) 
		or (NameMap.find wid pglobals = String("Fixed")) then
		[Cast.Expr (Call("gtk_fixed_put",
	[Cast.Call("GTK_FIXED",[Id wid]); Id id; Literal x; Literal y]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a Fixed widget."))
		| (_, "BoxPack", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_box_pack_start",
	[Call("GTK_BOX",[Id id]); Id wid; ConstLit("FALSE"); ConstLit("FALSE"); Literal 10]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "ClickQuit", _) ->
	[Cast.Expr (Call("g_signal_connect",
	[Call("G_OBJECT",[Id id]); StrLit "activate"; Call("G_CALLBACK",[ConstLit("gtk_main_quit")]); Null]))]
		| (_, "ClickDisplay", [Ast.Call (fid, [Ast.Id wid; _])]) ->
	[Cast.Expr (Call("g_signal_connect",
	[Call("G_OBJECT",[Id id]); StrLit "clicked"; Call("G_CALLBACK",[Id fid]); Id wid]))]
		| (_, "BorderWidth", [Ast.Literal w]) ->
	[Cast.Expr (Call("gtk_container_set_border_width",
	[Call("GTK_CONTAINER",[Id id]); Literal w]))]
		| (String("Menuitem"), "Label", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_menu_item_set_label",
	[Call("GTK_MENU_ITEM",[Id id]); StrLit s])))]
		| (_, "InMenu", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_menu_item_set_submenu",
	[Call("GTK_MENU_ITEM",[Id id]); Id wid]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "AppendToMenu", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_menu_shell_append",
	[Call("GTK_MENU_SHELL",[Id wid]); Id id]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "Show", _) ->
	[Cast.Expr (Call("gtk_widget_show",[Id id]))]
		| (_, "Showall", _) ->
	[Cast.Expr (Call("gtk_widget_show_all",[Id id]))]



		| (_, _, _) -> raise (Failure ("Error: Variable " ^ id ^ "or object " ^ obj ^" is invalid or cannot be changed."))
		 

		in
                 match lfdecl.body with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print
           )} in
         let ncenv = 
           ( match f with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> List.rev([nfdecl]@(List.tl (List.rev x))))
	in (env, (s, g, ncenv)))


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
        returnType = BasicType(Cast.Int);
        fname = "main";
        formals = [FormalDecl (BasicType (Cast.Int), "argc");
   		   FormalDecl (PointerToPointerType (Cast.Char), "argv")];
        locals = [];
        body = [];
    } in

    (* Execute each statement in sequence, return updated global symbol table *)
    let env, cenv = (List.fold_left exec ((locals,globals), ([], [], [new_cenv]))
    odecl.obody) in
    (env, cenv)
  
  (* Run a program: initialize global variables to 0, find and run "main" *)
  in let globals = List.fold_left
      (fun globals vdecl -> NameMap.add vdecl (Int 0) globals) NameMap.empty vars
  in try
      let env, cenv = call (NameMap.find "Main" obj_decls) [] globals in
      let csdecls, cvdecls, cfdecls = cenv in
      let lfdecl = List.hd (List.rev cfdecls) in
      let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; locals =
        lfdecl.locals; body = (

        (* Append "return 0;" as the last body declaration *)
        let gtkinit = [Cast.Expr(Call("\n\tgtk_init", [ConstLit "&argc"; ConstLit "&argv"]))] 
	and gtkmain_return = [Cast.Expr(Call("gtk_main",[]));Cast.Return(Literal 0)]  in
        match lfdecl.body with
        []  -> gtkinit @ gtkmain_return
      | [x] -> gtkinit @ [x] @gtkmain_return
      | x   -> gtkinit @ x @ gtkmain_return
        ) } in

      (* Append to the new cenv *)
      let ncenv = 
      ( match cfdecls with
          []  ->     []
        | [x] ->     [nfdecl]
        | x   -> List.rev([nfdecl]@(List.tl (List.rev x)))) in  
      let listing = Cast.string_of_program (csdecls, cvdecls, ncenv) in

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
