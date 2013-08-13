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
    let rec eval env : (Ast.expr * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list) * Ast.obj_decl NameMap.t) ->
        (primitive * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list) * Ast.obj_decl NameMap.t) * (primitive NameMap.t * primitive NameMap.t) 
        = function

        (* Primitive expressions *)
	(Ast.Literal(i), cenv, loc_obj_decls) -> (Int i, cenv, loc_obj_decls), env
      | (Ast.StrLit(i), cenv, loc_obj_decls) -> (String i, cenv, loc_obj_decls), env
      | (Ast.CharLit(i), cenv, loc_obj_decls) -> (Char i, cenv, loc_obj_decls), env

        (* Other expressions *)
      | (Ast.Noexpr, cenv, loc_obj_decls) -> (Int 1, cenv, loc_obj_decls), env (* must be non-zero for the for loop predicate *)

      | (Ast.Id(var), cenv, loc_obj_decls) ->
      let locals, globals = env in
	  if NameMap.mem var locals then
	    ((NameMap.find var locals), cenv, loc_obj_decls), env
	  else if NameMap.mem var globals then
	    ((NameMap.find var globals), cenv, loc_obj_decls), env
	  else raise (Failure ("undeclared identifier " ^ var))

      | (Ast.Binop(e1, op, e2), cenv, loc_obj_decls) ->
	  let (v1, cenv, loc_obj_decls), env = eval env (e1, cenv, loc_obj_decls) in
          let (v2, cenv, loc_obj_decls), env = eval env (e2, cenv, loc_obj_decls) in
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
	               | Ast.Geq -> boolean (x1 >= x2)), cenv, loc_obj_decls)
             | _ -> raise (Failure("Error: invalid operation on a binary expression."))), env

      | (Ast.ChildId(var, subvar), cenv, loc_obj_decls) ->
        (* Find pointer variable *)
        let (locals, globals) = env in
        if NameMap.mem var locals then
            let obj_id = (match (NameMap.find var locals) with
              Pointer(x) -> x
            | _ -> raise(Failure("not a pointer type"))) in
	      if NameMap.mem obj_id loc_obj_decls then 
            (* Get the object declaration *)
            let odecl = NameMap.find obj_id loc_obj_decls in
            
            (* Find the assignment *)
            (* Find user-defined variable if it exists *)
            let oexpr = List.filter (fun e ->
              match e with
                Ast.Expr(x) -> (match x with
                                Ast.Assign(xx, _) -> if xx = ("%" ^ subvar) then true else false
                                | _ -> false)
              | _ -> false
            ) odecl.obody in

            (* Otherwise, get the originally defined variable *)
            let oexpr = if List.length oexpr > 0 then List.hd oexpr else begin
              List.hd (List.filter (fun e ->
              match e with
                Ast.Expr(x) -> (match x with
                                Ast.Assign(xx, _) -> if xx = subvar then true else false
                                | _ -> false)
              | _ -> false
            ) odecl.obody) end in
            let result = ( match oexpr with
                Ast.Expr(x) -> ( match x with
                    Ast.Assign(subvar, ex) -> 
                        let (value, cenv, new_obj_decls), (locals, globals) = eval env
                        (ex, cenv, loc_obj_decls) in 
                        ignore(match value with Int(x) -> print_endline ("val:" ^
                        (string_of_int x)) | _ -> ());value
                  | _ -> raise(Failure("undeclared identifier " ^ subvar ^ " for " ^ var)) )
              | _ -> raise(Failure("undeclared identifier " ^ subvar ^ " for " ^ var)) ) in

            (* Return value *)
            (result, cenv, loc_obj_decls), env 
          else 
            begin raise (Failure (var ^ " references undefined local function
              " ^ obj_id)) end
        else 
          begin raise (Failure ("undefined reference " ^ var)) end 

      | (Ast.ChildAssign(var, subvar, e), cenv, loc_obj_decls) ->
	    let (value, cenv, loc_obj_decls), (locals, globals) = eval env (e, cenv, loc_obj_decls) in
        let new_loc_obj_decls =
          if NameMap.mem var locals then
            let obj_id = (match (NameMap.find var locals) with
              Pointer(x) -> x
            | _ -> raise(Failure("not a pointer type"))) in
	      try 
            (* Get the object declaration *)
            let odecl = NameMap.find obj_id loc_obj_decls in
            
            (* Remove previous declaration *)
            let new_body = List.filter (fun e -> match e with
                Ast.Expr(x) -> (match x with
                                  Ast.Assign(i, _) -> 
                                    if i = ("%" ^ subvar) then false else true
                                | _ -> true)
              | _ -> false) odecl.obody in

            (* Create variable assignment %var *)
            let assign_var = ( match value with
                Int(x) -> Ast.Expr (Ast.Assign("%" ^ subvar, (Ast.Literal x))) 
              | String(x) -> Ast.Expr (Ast.Assign("%" ^ subvar, (Ast.StrLit x)))
              | Char(x) -> Ast.Expr (Ast.Assign("%" ^ subvar, (Ast.CharLit x)))
              | _ -> raise(Failure("cannot assign a non-primitive")) ) in

            (* Append to final body's expression list *)
            let final_body = assign_var::new_body in
            let new_decl = { oname = odecl.oname; oformals =
              odecl.oformals; olocals = odecl.olocals; obody =
              final_body } in

            (* Return value *)
            (NameMap.add obj_id new_decl loc_obj_decls)
	      with Not_found -> raise (Failure ("undefined function " ^ obj_id))
          else raise (Failure ("undefined reference " ^ subvar ^ " for " ^ var))              
        in
        (Int 1, cenv, new_loc_obj_decls), (locals, globals)

      | (Ast.Assign(var, e), cenv, loc_obj_decls) ->
	      let (v, cenv, loc_obj_decls), (locals, globals) = eval env (e, cenv, loc_obj_decls) in

            (* No variable declaration necessary: the 'else' just creates the new variable *)
            (match v with
               Int(x) ->
	             if NameMap.mem var locals then

                   (* See if it's calling a local object *)
                   if NameMap.mem odecl.oname loc_obj_decls then 
               
                     (* Remove previous declaration *)
                     let curr_decl = NameMap.find odecl.oname loc_obj_decls in
                     let new_body = List.filter (fun e -> match e with
                       Ast.Expr (Ast.Assign(var, e)) -> false
                         | _ -> true) curr_decl.obody in

                     (* Create variable assignment %var *)
                     let assign_var = Ast.Expr (Ast.Assign("%" ^ var, (Ast.Literal x))) in

                     (* Append to final body's expression list *)
                     let final_body = assign_var::new_body in

                     let new_decl = { oname = curr_decl.oname; oformals =
                       curr_decl.oformals; olocals = curr_decl.olocals; obody =
                         final_body } in

	                 (*return Int 1? since assign succeeds?*) 
                     (Int x, cenv, (NameMap.add odecl.oname new_decl loc_obj_decls)), 
                       (NameMap.add var v locals, globals)
                   else begin
                     (Int x, cenv,
                       loc_obj_decls), (NameMap.add var v locals, globals)
                   end

	             else if NameMap.mem var globals then
	               (*Make decision on Int 1 vs x*) 
                   (Int x, cenv, loc_obj_decls), (locals, NameMap.add var v globals)

	             else begin 
                   (* See if it's calling a local object *)
                   if NameMap.mem odecl.oname loc_obj_decls then 
               
                     (* Remove previous declaration *)
                     let curr_decl = NameMap.find odecl.oname loc_obj_decls in
                     let new_body = List.filter (fun e -> match e with
                       Ast.Expr (Ast.Assign(var, e)) -> false
                         | _ -> true) curr_decl.obody in

                     (* Create variable assignment %var *)
                     let assign_var = Ast.Expr (Ast.Assign("%" ^ var, (Ast.Literal x))) in

                     (* Append to final body's expression list *)
                     let final_body = assign_var::new_body in

                     let new_decl = { oname = curr_decl.oname; oformals =
                       curr_decl.oformals; olocals = curr_decl.olocals; obody =
                         final_body } in

	                 (*return Int 1? since assign succeeds?*) 
                     (Int x, cenv, (NameMap.add odecl.oname new_decl loc_obj_decls)), 
                       (NameMap.add var v locals, globals)
                   else begin
                     (Int x, cenv,
                       loc_obj_decls), (NameMap.add var v locals, globals)
                   end
                 end
             | String(x) ->
	         if NameMap.mem var locals then
	          (*return Int 1? since assign succeeds?*) (String x, cenv,
                  loc_obj_decls), (NameMap.add var v locals, globals)
	         else if NameMap.mem var globals then
	           (*Make decision on Int 1 vs x*) (String x, cenv,
                   loc_obj_decls), (locals, NameMap.add var v globals)
	         else (String x, cenv, loc_obj_decls), (NameMap.add var v locals, globals) 
             | Char(x) ->
	         if NameMap.mem var locals then
	          (*return Int 1? since assign succeeds?*) (Char x, cenv,
                  loc_obj_decls), (NameMap.add var v locals, globals)
	         else if NameMap.mem var globals then
	           (*Make decision on Int 1 vs x*) (Char x, cenv, loc_obj_decls), (locals, NameMap.add var v globals)
	         else (Char x, cenv, loc_obj_decls), (NameMap.add var v locals, globals) 
             | _ -> raise (Failure ("Error: Cannot assign type")))

      | (Ast.AssignObj(i, f, actuals), cenv, loc_obj_decls) ->
              (* Find the object declaration first *)
              let odecl =
                  try NameMap.find f obj_decls
                  with Not_found -> raise (Failure ("undefined function " ^ f))
              in

              (* Create new object declaration *)
              let new_odecl = { oname = ("$" ^ i); oformals = odecl.oformals;
              olocals = odecl.olocals; obody = odecl.obody } in

              (* Replace variable i *)
	          let (locals, globals) = env in
              let new_env = (
                if NameMap.mem i locals then
                  NameMap.add i (Pointer ("$" ^ i)) locals, globals
	            else if NameMap.mem i globals then
	              locals, NameMap.add i (Pointer ("$" ^ i)) globals
	            else 
                  NameMap.add i (Pointer ("$" ^ i)) locals, globals ) in

              (* Return new enviornment *)
              let new_obj_decls =(NameMap.add ("$" ^ i) new_odecl loc_obj_decls)
              in
              (Int 1, cenv, new_obj_decls), new_env
                  

      | (Ast.Call("puts", [e]), cenv, loc_obj_decls) ->
	  let (v, (cstrs, cvars, cfuncs), loc_obj_decls), env = eval env (e, cenv, loc_obj_decls) in

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
                  | Pointer(x) -> 
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
           | Pointer(x) -> print_endline x
           | Char(x) -> print_endline (String.make 1 x));

           (* Return the new environment *)
	   ((Int 0), (cstrs, cvars, ncenv), loc_obj_decls), env

      | (Ast.Call(f, actuals), cenv, loc_obj_decls) ->
        let (locals, globals) = env in
        let odecl =
          if NameMap.mem f locals then
            let obj_id = (match (NameMap.find f locals) with
              Pointer(x) -> x
            | _ -> raise(Failure("not a pointer type"))) in
	      try NameMap.find obj_id loc_obj_decls
	      with Not_found -> raise (Failure ("undefined function " ^ obj_id))
        else if NameMap.mem f globals then
          let obj_id = (match (NameMap.find f globals) with
              Pointer(x) -> x
            | _ -> raise(Failure("not a pointer type"))) in
	      try NameMap.find obj_id loc_obj_decls
	      with Not_found -> raise (Failure ("undefined function " ^ obj_id))
        else raise (Failure ("undefined reference " ^ f)) 
	    in
	    let actuals, env = List.fold_left
	      (fun (actuals, env) actual ->
                  let (v, cenv, temp_obj_decls), env = eval env (actual, cenv,
                  loc_obj_decls) in v :: actuals, env)
   	      ([], env) (List.rev actuals)
	    in
	    try
            (* The inner (_, globals) ignores locals, and the outer ( ,_) ignores cenv *)
	      let ((_, globals), _, temp_obj_decls) = call odecl actuals globals
	      in ((Int 0), cenv, loc_obj_decls), (locals, globals)
	    with ReturnException(v, globals) -> (v, cenv, loc_obj_decls), (locals, globals)
    in

    (* Execute a statement and return an updated environment *)
    let rec exec (env, cenv, loc_obj_decls) : Ast.stmt -> ((primitive NameMap.t * primitive
    NameMap.t) * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list) * Ast.obj_decl
    NameMap.t) = function
	Ast.Block(stmts) -> List.fold_left exec (env, cenv, loc_obj_decls) stmts
      | Ast.Expr(e) -> let (_, cenv, loc_obj_decls), env = eval env (e, cenv, loc_obj_decls) in (env, cenv, loc_obj_decls)
      | Ast.If(e, s1, s2) ->
              let (v, cenv, _), env = eval env (e, cenv, loc_obj_decls) in
            (match v with
              Int(x) ->
	          exec (env, cenv, loc_obj_decls) (if x != 0 then s1 else s2)
            | _ -> raise (Failure ("Error: invalid operation on a conditional
            statement.")))
      | Ast.While(e, s) ->
	  let rec loop (env, cenv, loc_obj_decls) =
              let (v, cenv, loc_obj_decls), env = eval env (e, cenv,
              loc_obj_decls) in
            (match v with
               Int(x) ->
	           if x != 0 then loop (exec (env, cenv, loc_obj_decls) s) else env
             | _ -> raise (Failure ("Error: invalid operation on a while
             statement.")) )
          in (loop (env, cenv, loc_obj_decls), cenv, loc_obj_decls)
      | Ast.For(e1, e2, e3, s) ->
              let (_, cenv, _), env = eval env (e1, cenv, loc_obj_decls) in
	  let rec loop env =
              let (v, cenv, _), env = eval env (e2, cenv, loc_obj_decls) in
            (match v with
            Int(x) ->
	    if x != 0 then
                let (env, cenv, loc_obj_decls) = (exec (env, cenv,
                loc_obj_decls) s) in 
                let (_, cenv, loc_obj_decls), env = eval env (e3, cenv,
                loc_obj_decls) in
	      loop (env)
	    else
	      env, cenv, loc_obj_decls
            | _ -> raise (Failure ("Error: invalid operation on a for statement.")))
	  in (loop env)
      | Ast.Return(e) ->
              let (v, cenv, _), (locals, globals) = eval env (e, cenv,
              loc_obj_decls) in
	  raise (ReturnException(v, globals))
      | Ast.Declare(o, v) -> 
              let (locals, globals) = env in 
              let var = ((NameMap.add v (Int 0) locals), globals) in 
              (var, cenv, loc_obj_decls)
      | Ast.Set(id, obj, args) -> 
	let (st, g, f) = cenv in
              let (plocals, pglobals) = env in 
  		if not (NameMap.mem id plocals or NameMap.mem id pglobals) then
(* Creates new gtk objects *)
 	(
		let var = ((NameMap.add id (String obj) plocals), pglobals) in 
		let lfdecl = List.hd (List.rev f) in
		let nfdecl = {  returnType = lfdecl.returnType; 
				fname = lfdecl.fname; 
				formals = lfdecl.formals; 
             			locals= (
	let print =
		(match (obj, args) with
(* Doesn't do anything for Display function types *)
			  ("Display", _) -> [] 
(* Declares array type widgets *)
			| ("CheckBoxArray", [Ast.Literal n]) ->
        [(Cast.OneDArrDecl (PointerType (GtkWidget), id, Binop(Literal n, Sub, Literal 1)))]
			| (_, _) ->
(* Declares basic type widgets *)
		[(Cast.VDecl (PointerType (GtkWidget), id))])   
				 in
                 match lfdecl.locals with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print 
           ) ; body= (
               let print = 
(* Assign to widgets using gtk functions *)
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
		| ("CheckBox", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_check_button_new", []))))]	
		| ("CheckBox", [Ast.StrLit s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_check_button_new_with_label", [StrLit s]))))]	
		| ("CheckBoxArray", [Ast.Literal n]) ->
	[Cast.For(Assign("int i", Literal 1), Binop(Id "i",Leq,Literal n), Assign("i", Binop(Id "i",Add,Literal 1)), Cast.Block[Expr(Assign(id ^ "[i-1]", Call("gtk_check_button_new", [])))])]
		| ("Menubar", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_bar_new", []))))]
		| ("Menu", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_new", []))))]
		| ("Menuitem", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_item_new", []))))]
		| ("Menuitem", [Ast.StrLit s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_item_new_with_label", [StrLit s]))))]
    	| ("TextEntry", []) ->
    [Cast.Expr (Assign (id, (Call ("gtk_entry_new", []))))]   
		| ("RadioButton", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_radio_button_new", [Null]))))] 
 		| ("ComboBox", []) ->
    [Cast.Expr (Assign (id, (Call ("gtk_combo_box_new", []))))]      
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
              (var, (st, g, ncenv), loc_obj_decls)
)
(* If id already exist, go to property settings *)
  		else(
(* Callback support *)
			match (obj, args) with
	("ClickDisplay", [Ast.Call (fid, [Ast.Id wid; Ast.StrLit s])])	-> 
(if ((NameMap.mem wid plocals) or (NameMap.mem wid pglobals)) 
then (
(* Widget to be changed exists *)
let addfdecl = {
		returnType = BasicType(Cast.Void);
        fname = fid;
        formals = [FormalDecl (PointerType (GtkWidget), "widget");
   		   FormalDecl (BasicType (GPointer), wid)];
        locals = [];
        body = (
		match ((if NameMap.mem fid plocals then NameMap.find fid plocals else NameMap.find fid pglobals), (if NameMap.mem wid plocals then NameMap.find wid plocals else NameMap.find wid pglobals), Ast.StrLit(s)) with
		  (String("Display"), String("Label"), Ast.StrLit(s)) ->
	[(Cast.Expr (Call("gtk_label_set_text ", [Call("GTK_LABEL",[Id wid]); StrLit s])))]
		
		| _ -> raise (Failure ("Error: Callback function not supported."))
		);}				
 in
		let lfdecl = List.hd (List.rev f) in
		let nfdecl = { returnType = lfdecl.returnType; 
			fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals; body= (
				let print =
	[Cast.Expr (Call("g_signal_connect",
	[Call("G_OBJECT",[Id id]); StrLit "clicked"; Call("G_CALLBACK",[Id fid]); Id wid]))]
 in
                 match lfdecl.body with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print
           )}
in let ncenv = 
           ( match f with
               []  ->     []
             | [x] -> [addfdecl] @ [nfdecl]
             | x   -> [addfdecl] @ List.rev([nfdecl]@(List.tl (List.rev x))))
	in (env, (st, g, ncenv), loc_obj_decls))
else raise (Failure ("Error: Unknown widget " ^ wid))
				)
(* Set properties *)
 			| _ -> (

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
		| (String("Checkbox"), "Label", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_button_set_label",
	[Cast.Call("GTK_BUTTON",[Cast.Id id]); StrLit s])))]		
		| (String("RadioButton"), "Label", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_button_set_label",
	[Cast.Call("GTK_BUTTON",[Cast.Id id]); StrLit s])))]
		| (String("TextEntry"), "Text", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_entry_set_text ",
	[Cast.Call("GTK_LABEL",[Cast.Id id]); StrLit s])))]	
		| (String("ComboBox"), "Append", [Ast.StrLit s]) ->
	[(Cast.Expr (Call("gtk_combo_box_append_text ",
	[Cast.Call("GTK_COMBO",[Cast.Id id]); StrLit s])))]	
		| (String ("Checkbox"), "Active", [Ast.Id("yes")]) ->		
	[Cast.Expr (Call("gtk_toggle_button_set_active",
	[Call("GTK_TOGGLE_BUTTON",[Id id]); ConstLit("TRUE")]))]
		| (String ("Checkbox"), "Active", [Ast.Id("no")]) ->		
	[Cast.Expr (Call("gtk_toggle_button_set_active",
	[Call("GTK_TOGGLE_BUTTON",[Id id]); ConstLit("FALSE")]))]	


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
	in (env, (st, g, ncenv), loc_obj_decls)))


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
    let env, cenv, new_loc_obj_decls = (List.fold_left exec ((locals,globals),
    ([], [], [new_cenv]), obj_decls) odecl.obody) in
    (env, cenv, new_loc_obj_decls)
  
  (* Run a program: initialize global variables to 0, find and run "main" *)
  in let globals = List.fold_left
      (fun globals vdecl -> NameMap.add vdecl (Int 0) globals) NameMap.empty vars
  in try
      let env, cenv, _ = call (NameMap.find "Main" obj_decls) [] globals in
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
