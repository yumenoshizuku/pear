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
  | Float of float
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
  let rec call odecl call_cenv actuals globals =

    (* Evaluate an expression and return (value, updated environment) *)
    let rec eval env : (Ast.expr * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list) * Ast.obj_decl NameMap.t) ->
        (primitive * (Cast.stru_def list * Cast.var_decl list * Cast.func_decl list) * Ast.obj_decl NameMap.t) * (primitive NameMap.t * primitive NameMap.t) 
        = function

        (* Primitive expressions *)
	(Ast.Literal(i), cenv, loc_obj_decls) -> (Int i, cenv, loc_obj_decls), env
	  | (Ast.FloLit(i), cenv, loc_obj_decls) -> (Float i, cenv, loc_obj_decls), env
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
			 | Float(x1), Float(x2) ->
			(Float (match op with
	                 Ast.Add -> x1 +. x2
	               | Ast.Sub -> x1 -. x2
	               | Ast.Mul -> x1 *. x2
	               | Ast.Div -> x1 /. x2
				   | _ -> raise (Failure("Error: invalid operation on floats."))), cenv, loc_obj_decls)
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
                        (ex, cenv, loc_obj_decls) in value
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
			  | Float(x) -> Ast.Expr (Ast.Assign("%" ^ subvar, (Ast.FloLit x))) 
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

(* The Ast.Set function for GTK is primarily written by Fanxing Meng, with occasional widgets (radio button arrays etc) by Rui Chen *)
      | (Ast.Set(id, obj, actuals), cenv, loc_obj_decls) -> 
	let (st, g, f) = cenv in
              let (plocals, pglobals) = env in 
  		if not (NameMap.mem id plocals or NameMap.mem id pglobals) then (* Creates new gtk objects *)
 	(
	    let args, env = List.fold_left
	      (fun (actuals, env) actual ->
                  let (v, cenv, temp_obj_decls), env = eval env (actual, cenv,
                  loc_obj_decls) in v :: actuals, env)
   	      ([], env) (List.rev actuals)
	    in
		let var = ((NameMap.add id (String obj) plocals), pglobals) in 
		let lfdecl = List.hd (List.rev f) in
		let nfdecl = {  returnType = lfdecl.returnType; 
				fname = lfdecl.fname; 
				formals = lfdecl.formals; 
             			locals= (
	let print =
		(match (obj, args) with
(* Declares array type widgets *)
			  ("CheckBoxArray", [Int n]) ->
        [(Cast.OneDArrDecl (PointerType (GtkWidget), id, Literal n))]
			| ("ButtonArray", [Int n]) ->
        [(Cast.OneDArrDecl (PointerType (GtkWidget), id, Literal n))]
			| ("ToolButton", _) ->
		[(Cast.VDecl (PointerType (GtkToolItem), id))]
(* Declares color type widgets *)
			| ("Color", _) ->
		[(Cast.VDecl (BasicType (GdkColor), "color"))]
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
		| ("Vbox", [String "hom"]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("TRUE")]))]
		| ("Vboxhom", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("TRUE")]))]
		| ("Vbox", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("FALSE")]))]
		| ("Box", [String "vertical"]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_VERTICAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("FALSE")]))]
		| ("Box", [String "horizontal"]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_box_new", [ConstLit("GTK_ORIENTATION_HORIZONTAL"); Literal 5]))));
	 Cast.Expr (Call("gtk_box_set_homogeneous",[Call("GTK_BOX",[Id id]);ConstLit("FALSE")]))]
		| ("Hbox", [String "hom"]) ->
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
		| ("Button", [String s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_button_new_with_label", [StrLit s]))))]
		| ("Label", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_label_new ", [Null]))))]
		| ("Label", [String s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_label_new ", [StrLit s]))))]
		| ("CheckBox", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_check_button_new", []))))]	
		| ("CheckBox", [String s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_check_button_new_with_label", [StrLit s]))))]	
		| ("CheckBoxArray", [Int n]) ->
	[Cast.For(Assign("int i", Literal 1), Binop(Id "i",Leq,Literal n), Assign("i", Binop(Id "i",Add,Literal 1)), Cast.Block[Expr(Assign(id ^ "[i-1]", Call("gtk_check_button_new", [])))])]
		| ("ButtonArray", [Int n]) ->
	[Cast.For(Assign("int i", Literal 1), Binop(Id "i",Leq,Literal n), Assign("i", Binop(Id "i",Add,Literal 1)), Cast.Block[Expr(Assign(id ^ "[i-1]", Call("gtk_button_new", [])))])]
(* Different types of tool buttons are built into pear *)
		| ("ToolButton", [String s]) ->
		(match s with
		  	  "about" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_ABOUT")])))]
		    | "add" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_ADD")])))]
		    | "apply" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_APPLY")])))]
		    | "cancel" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_CANCEL")])))]
		    | "cdrom" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_CDROM")])))]
		    | "clear" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_CLEAR")])))]
		    | "close" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_CLOSE")])))]
		    | "copy" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_COPY")])))]
		    | "cut" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_CUT")])))]
		    | "paste" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_PASTE")])))]
		    | "delete" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_DELETE")])))]
		    | "error" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_DIALOG_ERROR")])))]
		    | "info" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_INFO")])))]
		    | "question" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_DIALOG_QUESTION")])))]
		    | "warning" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_DIALOG_WARNING")])))]
		    | "folder" -> [Cast.Expr (Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_DIRECTORY")]))]
		    | "edit" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_EDIT")])))]
		    | "execute" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_EXECUTE")])))]
		    | "file" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_FILE")])))]
		    | "find" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_FIND")])))]
		    | "fullscreen" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_FULLSCREEN")])))]
		    | "back" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_GO_BACK")])))]
		    | "forward" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_GO_FORWARD")])))]
		    | "home" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_HOME")])))]
			| "network" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_NETWORK")])))]
		    | "new" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_NEW")])))]
		    | "open" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_OPEN")])))]
		    | "preference" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_PREFERENCES")])))]
		    | "print" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_PRINT")])))]
		    | "properties" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_PROPERTIES")])))]
		    | "quit" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_QUIT")])))]
		    | "refresh" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_REFRESH")])))]
		    | "save" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_SAVE")])))]
		    | "stop" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_STOP")])))]
		    | "undo" -> [Cast.Expr (Assign (id, Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_UNDO")])))]
		    | "redo" -> [Cast.Expr (Assign (id,Call("gtk_tool_button_new_from_stock",[ConstLit ("GTK_STOCK_REDO")])))]
			| "separator" -> [Cast.Expr (Assign (id, Call("gtk_separator_tool_item_new",[])))]
			| _ -> raise (Failure ("Error: Tool Button " ^ s ^ " is invalid.")))
		| ("Menubar", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_bar_new", []))))]
		| ("Menu", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_new", []))))]
		| ("Menuitem", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_item_new", []))))]
		| ("Menuitem", [String s]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_menu_item_new_with_label", [StrLit s]))))]
    	| ("TextEntry", []) ->
    [Cast.Expr (Assign (id, (Call ("gtk_entry_new", []))))]   
		| ("RadioButton", []) ->
	[Cast.Expr (Assign (id, (Call ("gtk_radio_button_new", [Null]))))] 
 		| ("ComboBox", []) ->
    [Cast.Expr (Assign (id, (Call ("gtk_combo_box_new", []))))]
		| ("Toolbar", []) ->
	 [Cast.Expr (Assign (id, (Call ("gtk_toolbar_new", []))))]
		| ("Statusbar", []) -> 
	 [Cast.Expr (Assign (id, (Call ("gtk_statusbar_new", []))))]
		| ("Image", [String s]) ->
	 [Cast.Expr (Assign (id, (Call ("gtk_image_new_from_file", [StrLit s]))))]
		| ("Align", [Float x; Float y; Float h; Float w]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_alignment_new", [FloLit x; FloLit y; FloLit h; FloLit w]))))]
		| ("Align", [Int x; Int y; Int h; Int w]) ->
	[Cast.Expr (Assign (id, (Call ("gtk_alignment_new", [Literal x; Literal y; Literal h; Literal w]))))]
		| _  -> raise (Failure ("Error: Object not supported."))) in
(* Prepend object initialization to the main function *)
                 match lfdecl.body with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print 
           )} in
(* Updates the main function *)
         let ncenv = 
           ( match f with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> List.rev([nfdecl]@(List.tl (List.rev x)))) in 
              (String(obj), (st, g, ncenv), loc_obj_decls), var
)
(* If widget id already exist, go to property settings *)
else (
(* The following three built-in callback function examples show how to deal with string manipulations (in an ugly way), which pear currently don't support. Later on there are elegant solutions to callback functions where all commands are accessible within pear/gtk *)
match (obj, actuals) with
          ("ClickIncrement",[Ast.Call (fid, [Ast.Id wid])]) -> (
if ((NameMap.mem wid plocals) or (NameMap.mem wid pglobals)) 
then (
(* Widget to be changed exists, wid is label *)
let addfdecl = {
		returnType = BasicType(Cast.Void);
        fname = fid;
        formals = [FormalDecl (PointerType (GtkWidget), "widget");
   		   FormalDecl (BasicType (GPointer), wid)];
        locals = [Cast.OneDArrDecl(BasicType(Cast.Char), "str", Literal 10)];
        body = (
		match (if NameMap.mem wid plocals then NameMap.find wid plocals else NameMap.find wid pglobals) with
		  String("Label") ->
	[Cast.Expr(Call("sprintf", [Id "str"; StrLit "%d"; Binop(Call("atoi", [Call("gtk_label_get_text", [Call("GTK_LABEL", [Id wid])])]) , Add, Literal 1) ])); Cast.Expr (Call("gtk_label_set_text ", [Call("GTK_LABEL",[Id wid]); Id "str"]	))]
		
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
	in (String(obj), (st, g, ncenv), loc_obj_decls), env)
else raise (Failure ("Error: Unknown widget " ^ wid))
)
(* Click decrement the number on label by 1 *)
			| ("ClickDecrement",[Ast.Call (fid, [Ast.Id wid])]) -> (
if ((NameMap.mem wid plocals) or (NameMap.mem wid pglobals)) 
then (
(* Widget to be changed exists, wid is label *)
let addfdecl = {
		returnType = BasicType(Cast.Void);
        fname = fid;
        formals = [FormalDecl (PointerType (GtkWidget), "widget");
   		   FormalDecl (BasicType (GPointer), wid)];
        locals = [Cast.OneDArrDecl(BasicType(Cast.Char), "str", Literal 10)];
        body = (
		match (if NameMap.mem wid plocals then NameMap.find wid plocals else NameMap.find wid pglobals) with
		  String("Label") ->
[Cast.Expr(Call("sprintf", [Id "str"; StrLit "%d"; Binop(Call("atoi", [Call("gtk_label_get_text", [Call("GTK_LABEL", [Id wid])])]) ,Sub, Literal 1) ])); Cast.Expr (Call("gtk_label_set_text ", [Call("GTK_LABEL",[Id wid]); Id "str"]	))]
		
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
	in (String(obj), (st, g, ncenv), loc_obj_decls), env)
else raise (Failure ("Error: Unknown widget " ^ wid))
)
			| ("Calcbutton",[Ast.Literal n; Ast.Call (fid, [Ast.Id wid])]) -> (
if ((NameMap.mem wid plocals) or (NameMap.mem wid pglobals)) 
then (
(* Widget to be changed exists, wid is label *)
let addfdecl = {
		returnType = BasicType(Cast.Void);
        fname = fid;
        formals = [FormalDecl (PointerType (GtkWidget), "widget");
   		   FormalDecl (BasicType (GPointer), wid)];
        locals = [Cast.OneDArrDecl(PointerType(Cast.GChar), "result", Literal 1)];
        body = (
		match (if NameMap.mem wid plocals then NameMap.find wid plocals else NameMap.find wid pglobals) with
		  String("Label") ->
	[Cast.If (
Cast.Call("g_str_has_prefix", [Call("gtk_button_get_label", [Call("GTK_BUTTON", [Id "widget"])]); StrLit "="]), 

Expr(Call("__returnValue__", [Id wid])), 

Cast.If(Call("g_file_get_contents", [StrLit "out.txt"; Id "result"; Null; Null]), 

Cast.Block[Cast.Expr(Call("gtk_label_set_text", [Call("GTK_LABEL", [Id wid]) ; Null]));
Cast.Expr(Call("g_remove", [StrLit "out.txt"]));
Cast.Expr(Call("gtk_label_set_text", [Call("GTK_LABEL", [Id wid]); Call("g_strconcat", [Call("gtk_label_get_text", [Call("GTK_LABEL",[Id wid])]); Call("gtk_button_get_label", [Call("GTK_BUTTON", [Id "widget"])]); Null])]))],
Cast.Expr(Call("gtk_label_set_text", [Call("GTK_LABEL",[Id wid]);Call("g_strconcat", [Call("gtk_label_get_text", [Call("GTK_LABEL", [Id wid])]) ; Call("gtk_button_get_label", [Call("GTK_BUTTON", [Id "widget"])]); Null])]))
)
)]
		| _ -> raise (Failure ("Error: Callback function not supported."))
		);}				
 in
		let lfdecl = List.hd (List.rev f) in
		let nfdecl = { returnType = lfdecl.returnType; 
			fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals; body= (
				let print =
	[Cast.For(Assign("int i", Literal 0), Binop(Id "i",Less,Literal n), Assign("i", Binop(Id "i",Add,Literal 1)), Expr(Call("g_signal_connect",
	[Call("G_OBJECT",[OneDArrSubs(Id id, Id "i")]); StrLit "clicked"; Call("G_CALLBACK",[Id fid]); Id wid])))]
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
	in (String(obj), (st, g, ncenv), loc_obj_decls), env)
else raise (Failure ("Error: Unknown widget " ^ wid))
)

| _ -> (
(* To deal with different types of callback functions that could have other "Set" nested in, the type of action os specified and a new function is created and prepended to the c program. *)
	match (obj, actuals) with
		      ("Clicked", [Ast.Id wid; _]) -> (

 let addfdecl = [{
		returnType = BasicType(Cast.Void);
        fname = id ^ "Clicked";
        formals = [FormalDecl (PointerType (GtkWidget), "widget");
   		   FormalDecl (BasicType (GPointer), wid)];
        locals = [];
        body = [];}]				
 in
 let (v, (addsdecl, addgdecl, addfdecl), temp_obj_decls), env = List.fold_left
	      (fun ((actuals, (addsdecl, addgdecl, addfdecl), loc_obj_decls), env) actual ->
                  let (v, (_, _, addfdecl), temp_obj_decls), env = eval env (actual, ([], [], addfdecl), loc_obj_decls) in (v :: actuals, (addsdecl, addgdecl, addfdecl), loc_obj_decls), env)
   	      (([], ([], [], addfdecl), loc_obj_decls), env) (List.rev actuals)
 in
		let lfdecl = List.hd (List.rev f) in
		let nfdecl = { returnType = lfdecl.returnType; 
			fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals; body= (
				let print =
	[Cast.Expr (Call("g_signal_connect",
	[Call("G_OBJECT",[Id id]); StrLit "clicked"; Call("G_CALLBACK",[Id (id ^ "Clicked")]); Id wid]))]
 in
                 match lfdecl.body with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print
           )}
in let ncenv = 
           ( match f with
               []  ->     []
             | [x] -> addfdecl @ [nfdecl]
             | x   -> addfdecl @ (List.tl  x))
	in (String(obj), (st, g, ncenv), loc_obj_decls), env
)
			| ("Enter", [Ast.Id wid; _]) -> (

 let addfdecl = [{
		returnType = BasicType(Cast.Void);
        fname = id ^ "Enter";
        formals = [FormalDecl (PointerType (GtkWidget), "widget");
   		   FormalDecl (BasicType (GPointer), wid)];
        locals = [];
        body = [];}]				
 in
 let (v, (addsdecl, addgdecl, addfdecl), temp_obj_decls), env = List.fold_left
	      (fun ((actuals, (addsdecl, addgdecl, addfdecl), loc_obj_decls), env) actual ->
                  let (v, (_, _, addfdecl), temp_obj_decls), env = eval env (actual, ([], [], addfdecl), loc_obj_decls) in (v :: actuals, (addsdecl, addgdecl, addfdecl), loc_obj_decls), env)
   	      (([], ([], [], addfdecl), loc_obj_decls), env) (List.rev actuals)
 in
		let lfdecl = List.hd (List.rev f) in
		let nfdecl = { returnType = lfdecl.returnType; 
			fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals; body= (
				let print =
	[Cast.Expr (Call("g_signal_connect",
	[Call("G_OBJECT",[Id id]); StrLit "enter"; Call("G_CALLBACK",[Id (id ^ "Enter")]); Id wid]))]
 in
                 match lfdecl.body with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print
           )}
in let ncenv = 
           ( match f with
               []  ->     []
             | [x] -> addfdecl @ [nfdecl]
             | x   -> addfdecl @ (List.tl  x))
	in (String(obj), (st, g, ncenv), loc_obj_decls), env
)
(* All other basic property setting functions *)
 			| _ -> (

         let lfdecl = List.hd (List.rev f) in
		 let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= 
(
	let print =
		(match (obj, actuals) with
		 ("Color", _) ->
		[(Cast.VDecl (BasicType (GdkColor), "color"))]
			| (_, _) -> []  )
				 in
                 match lfdecl.locals with
                   []  ->     print
                 | [x] ->  x::print
                 | x   -> x @ print 
           )
; body= (
               let print = 
		match ((if NameMap.mem id plocals then NameMap.find id plocals else NameMap.find id pglobals), obj, actuals) with
(* If a widget is passed in as an argument, the argument should not be evaluated (because otherwise it would return the type of the widget, which has no use). *)
		  (String("Toolbar"), "Insert", [Ast.Id wid]) ->
	[(Cast.Expr (Call("gtk_toolbar_insert",
	[Call("GTK_TOOLBAR",[Cast.Id id]); Call("GTK_TOOL_ITEM",[Cast.Id wid]); Literal (-1)])))]
		| (String("Grid"), "AttachAt", [Ast.Id wid; Ast.Literal x; Ast.Literal y; Ast.Literal c; Ast.Literal r]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_grid_attach",
	[Call("GTK_GRID",[Id id]); Id wid; Literal x; Literal y; Literal c; Literal r]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (String("ButtonArray"), "AttachTo", [Ast.Literal i; Ast.Id wid; Ast.Literal x; Ast.Literal y; Ast.Literal c; Ast.Literal r]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_grid_attach",
	[Call("GTK_GRID",[Id wid]); OneDArrSubs(Id id, Literal i); Literal x; Literal y; Literal c; Literal r]))]
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
	[Call("GTK_FIXED",[Id wid]); Id id; Literal x; Literal y]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a Fixed widget."))
		| (_, "BoxPack", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_box_pack_start",
	[Call("GTK_BOX",[Id id]); Id wid; ConstLit("FALSE"); ConstLit("FALSE"); Literal 5]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "BoxPackFill", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_box_pack_start",
	[Call("GTK_BOX",[Id id]); Id wid; ConstLit("FALSE"); ConstLit("TRUE"); Literal 5]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "BoxPackExpand", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_box_pack_start",
	[Call("GTK_BOX",[Id id]); Id wid; ConstLit("TRUE"); ConstLit("FALSE"); Literal 5]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))
		| (_, "BoxPackFillExpand", [Ast.Id wid]) ->
		if (NameMap.mem wid plocals) or (NameMap.mem wid pglobals) then 
	[Cast.Expr (Call("gtk_box_pack_start",
	[Call("GTK_BOX",[Id id]); Id wid; ConstLit("TRUE"); ConstLit("TRUE"); Literal 5]))]
		else raise (Failure ("Error: " ^ wid ^ " is not a valid widget."))

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
		| _ -> (

	    let args, env = List.fold_left
	      (fun (actuals, env) actual ->
                  let (v, cenv, temp_obj_decls), env = eval env (actual, cenv,
                  loc_obj_decls) in v :: actuals, env)
   	      ([], env) (List.rev actuals)
	    in
	match ((if NameMap.mem id plocals then NameMap.find id plocals else NameMap.find id pglobals), obj, args) with
(* If there are no widgets passed in as argument, the args would be evaluated so that variables can be passed in as arguments. *)
		  (String("Window"), "Title", [String s]) ->
	[Cast.Expr (Call("gtk_window_set_title",
	[Call("GTK_WINDOW",[Id id]); StrLit s]))]
		| (String("Window"), "Position", [String "center"]) ->
	[Cast.Expr (Call("gtk_window_set_position",
	[Call("GTK_WINDOW",[Id id]); ConstLit("GTK_WIN_POS_CENTER")]))]
		| (String("Window"), "Move", [Int x; Int y]) ->
	[Cast.Expr (Call("gtk_window_move",
	[Call("GTK_WINDOW",[Id id]); Literal x; Literal y]))]
		| (String("Window"), "Size", [Int w; Int h]) ->
	[Cast.Expr (Call("gtk_window_set_default_size",
	[Call("GTK_WINDOW",[Id id]); Literal w; Literal h]))]
		| (_, "Size", [Int w; Int h]) ->
	[Cast.Expr (Call("gtk_widget_set_size_request",
	[Id id; Literal w; Literal h]))]
		| (String("Window"), "Resizable", [String "no"]) ->
	[Cast.Expr (Call("gtk_window_set_resizable",
	[Call("GTK_WINDOW",[Id id]); ConstLit("TRUE")]))]
		| (String("Frame"), "Label", [String s]) ->
	[(Cast.Expr (Call("gtk_frame_set_label",
	[Call("GTK_FRAME",[Cast.Id id]); StrLit s])))]
		| (String("Toolbar"), "Style", [String "icon"]) ->
	[(Cast.Expr (Call("gtk_toolbar_set_style",
	[Call("GTK_TOOLBAR",[Cast.Id id]); ConstLit ("GTK_TOOLBAR_ICONS")])))]
		| (String("Toolbar"), "Style", [String "text"]) ->
	[(Cast.Expr (Call("gtk_toolbar_set_style",
	[Call("GTK_TOOLBAR",[Cast.Id id]); ConstLit ("GTK_TOOLBAR_TEXT")])))]
		| (String("Toolbar"), "Style", [String "both"]) ->
	[(Cast.Expr (Call("gtk_toolbar_set_style",
	[Call("GTK_TOOLBAR",[Cast.Id id]); ConstLit ("GTK_TOOLBAR_BOTH")])))]
		| (String("Statusbar"), "Show", [String s]) ->
	[(Cast.Expr (Call("gtk_statusbar_push",
	[Call("GTK_STATUSBAR",[Cast.Id id]); Call("gtk_statusbar_get_context_id",[Call("GTK_STATUSBAR",[Cast.Id id]); StrLit ""]); StrLit s])))]
		| (String("Button"), "Label", [String s]) ->
	[(Cast.Expr (Call("gtk_button_set_label",
	[Call("GTK_BUTTON",[Cast.Id id]); StrLit s])))]
		| (String("ButtonArray"), "Label", [Int i; String s]) ->
	[(Cast.Expr (Call("gtk_button_set_label",
	[Call("GTK_BUTTON",[Cast.OneDArrSubs(Id id, Literal i)]); StrLit s])))]
		| (String("Label"), "Text", [String s]) ->
	[(Cast.Expr (Call("gtk_label_set_text",
	[Call("GTK_LABEL",[Cast.Id id]); StrLit s])))]
		| (String("Label"), "GetText", []) ->
	[(Cast.Expr (Call("gtk_label_get_text",
	[Call("GTK_LABEL",[Cast.Id id])])))]
		| (String("Frame"), "Shadow", [String s]) ->
		(match s with
		  	"in" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_IN")]))]
			| "out" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_OUT")]))]
			| "etched in" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_ETCHED_IN")]))]
			| "etched out" -> [Cast.Expr (Call("gtk_frame_set_shadow_type",
		[Call("GTK_FRAME",[Id id]); ConstLit ("GTK_SHADOW_ETCHED_OUT")]))]
			| _ -> raise (Failure ("Error: Shadow " ^ s ^ " is invalid.")))		
		| (String("Grid"), "InsertRowAt", [Int r]) ->
	[Cast.Expr (Call("gtk_grid_insert_row",
	[Call("GTK_GRID",[Id id]); Literal r]))]
		| (String("Grid"), "InsertColumnAt", [Int c]) ->
	[Cast.Expr (Call("gtk_grid_insert_column",
	[Call("GTK_GRID",[Id id]); Literal c]))]
		| (String("Grid"), "Hom", [String "row"]) ->
	[Cast.Expr (Call("gtk_grid_set_row_homogeneous",
	[Call("GTK_GRID",[Id id]); ConstLit("TRUE")]))]
		| (String("Grid"), "Hom", [String "column"]) ->
	[Cast.Expr (Call("gtk_grid_set_column_homogeneous",
	[Call("GTK_GRID",[Id id]); ConstLit("TRUE")]))]
		| (_, "ClickQuit", _) ->
	[Cast.Expr (Call("g_signal_connect",
	[Call("G_OBJECT",[Id id]); StrLit "activate"; Call("G_CALLBACK",[ConstLit("gtk_main_quit")]); Null]))]
		| (_, "BorderWidth", [Int w]) ->
	[Cast.Expr (Call("gtk_container_set_border_width",
	[Call("GTK_CONTAINER",[Id id]); Literal w]))]
		| (String("Menuitem"), "Label", [String s]) ->
	[(Cast.Expr (Call("gtk_menu_item_set_label",
	[Call("GTK_MENU_ITEM",[Id id]); StrLit s])))]
		| (_, "Color", [Int rd; Int gr; Int bl]) ->
	[Cast.Expr(Assign("color.red", Literal rd));
	Expr(Assign("color.green", Literal gr));
	Expr(Assign("color.blue", Literal bl));
	Expr(Call("gtk_widget_modify_bg", [Id id; ConstLit "GTK_STATE_PRELIGHT"; ConstLit "&color"]))]
		| (_, "Show", _) ->
	[Cast.Expr (Call("gtk_widget_show",[Id id]))]
		| (_, "Showall", _) ->
	[Cast.Expr (Call("gtk_widget_show_all",[Id id]))]
		| (String("CheckBox"), "Label", [String s]) ->
	[(Cast.Expr (Call("gtk_button_set_label",
	[Call("GTK_BUTTON",[Cast.Id id]); StrLit s])))]		
		| (String("RadioButton"), "Label", [String s]) ->
	[(Cast.Expr (Call("gtk_button_set_label",
	[Call("GTK_BUTTON",[Cast.Id id]); StrLit s])))]
		| (String("TextEntry"), "Text", [String s]) ->
	[(Cast.Expr (Call("gtk_entry_set_text ",
	[Call("GTK_LABEL",[Cast.Id id]); StrLit s])))]	
		| (String("ComboBox"), "Append", [String s]) ->
	[(Cast.Expr (Call("gtk_combo_box_append_text ",
	[Call("GTK_COMBO",[Cast.Id id]); StrLit s])))]	
		| (String ("CheckBox"), "Active", [String "yes"]) ->		
	[Cast.Expr (Call("gtk_toggle_button_set_active",
	[Call("GTK_TOGGLE_BUTTON",[Id id]); ConstLit("TRUE")]))]
		| (String ("CheckBox"), "Active", [String "no"]) ->		
	[Cast.Expr (Call("gtk_toggle_button_set_active",
	[Call("GTK_TOGGLE_BUTTON",[Id id]); ConstLit("FALSE")]))]	

		| (_, _, _) -> raise (Failure ("Error: Variable " ^ id ^ "or object " ^ obj ^" is invalid or cannot be changed.")))
		 

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
	in (String(obj), (st, g, ncenv), loc_obj_decls),env)))                         

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
				  | Float(x) ->
					  Cast.Expr (Call("printf", [StrLit "%f\\n"; FloLit x]))
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

          (* Interpret if switched on *)
          if (Array.length Sys.argv > 1) then 
            if(Sys.argv.(1) = "-i") then  
              (match v with
                 Int(x) -> print_endline (string_of_int x)
		   | Float(x) -> print_endline (string_of_float x)
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
	      let ((locals, globals), _, loc_obj_decls) = call odecl call_cenv actuals globals
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
    let env, cenv, new_loc_obj_decls = (List.fold_left exec ((locals,globals),
    call_cenv, obj_decls) odecl.obody) in
    (env, cenv, new_loc_obj_decls)
  
  (* Run a program: initialize global variables to 0, find and run "main" *)
  in let globals = List.fold_left
      (fun globals vdecl -> NameMap.add vdecl (Int 0) globals) NameMap.empty vars
  in try
      (* Create the main method *)
      let new_cenv = {
        returnType = BasicType(Cast.Int);
        fname = "main";
        formals = [FormalDecl (BasicType (Cast.Int), "argc");
   		   FormalDecl (PointerToPointerType (Cast.Char), "argv")];
        locals = [];
        body = [];
      } in
      let env, cenv, _ = call (NameMap.find "Main" obj_decls) ([], [], [new_cenv]) [] globals in

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
						"#include <stdlib.h>\n" ^
                        "#include <gtk/gtk.h>\n" ^ 
						"#include <glib.h>\n" ^
						"#include <glib/gstdio.h>\n" ^
						listing ); 
  with Not_found -> raise (Failure ("did not find the main() function"))

(* Lex, parse and run program *)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  run program 
