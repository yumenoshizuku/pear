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
                   [] 	-> [print]
                 | [x] 	-> x::[print]
                 | x 	-> x @ [print]
           ) } in
         let ncenv =
           ( match cenv with
               [] 	-> []
             | [x] 	-> [nfdecl]
             | x 	-> x @ [nfdecl]) in
         (v1, (cvars, ncenv)), env 

 | (Ast.Window (id), cenv) ->
         let lfdecl = List.hd (List.rev (snd cenv)) in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= (
               let print =
                 (Cast.VDecl (Cast.PointerType (Cast.GtkWidget), id))   
				 in
                 match lfdecl.locals with
                   []  ->     [print]
                 | [x] ->  x::[print]
                 | x   -> x @ [print] 
           ) ; body= (
               let print = 
                 ( Cast.Expr (Cast.Assign (id, (Call ("gtk_window_new", [Cast.StrLit ("GTK_WINDOW_TOPLEVEL")]))))) and show = (Cast.Expr(Call("gtk_widget_show_all",[Cast.Id id]))) and destroy = Cast.Expr (Call("g_signal_connect",[Cast.Id id; Cast.StrLit ("\"destroy\"") ; Cast.Call("G_CALLBACK",[Cast.StrLit ("gtk_main_quit")]); Cast.Null])) in
                 match lfdecl.body with
                   []  ->     print::show::[destroy]
                 | [x] ->  x::print::show::[destroy]
                 | x   -> x @ [print] @ [show] @ [destroy]
           )} in
         let ncenv = 
           ( match (snd cenv) with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (String id, ((fst cenv), ncenv)), env  
 
  | (Ast.Create (id, typ), cenv) ->
         let lfdecl = List.hd (List.rev (snd cenv)) in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= (
               let print =
                 (Cast.VDecl (Cast.PointerType (Cast.GtkWidget), id))   
				 in
                 match lfdecl.locals with
                   []  ->     [print]
                 | [x] ->  x::[print]
                 | x   -> x @ [print] 
           ) ; body= (
               let print = 
               match typ with
	  "Fixed" ->
	( Cast.Expr (Cast.Assign (id, (Call ("gtk_fixed_new", []))))) 
	| "Frame" ->
	( Cast.Expr (Cast.Assign (id, (Call ("gtk_frame_new", [Cast.Null])))))
	| "Grid" ->
	 ( Cast.Expr (Cast.Assign (id, (Call ("gtk_grid_new", [])))))
	| "Vbox" ->
	( Cast.Expr (Cast.Assign (id, (Call ("gtk_box_new", [StrLit("GTK_ORIENTATION_HORIZONTAL"); Literal 10])))))
	| "Menubar" ->
	( Cast.Expr (Cast.Assign (id, (Call ("gtk_menu_bar_new", [])))))
	| "Menu" ->
	( Cast.Expr (Cast.Assign (id, (Call ("gtk_menu_new", [])))))
	| "Menuitem" ->
	( Cast.Expr (Cast.Assign (id, (Call ("gtk_menu_item_new", [])))))
	| _ -> raise (Failure ("Error: Widget " ^ typ ^ " not supported."))
		in match lfdecl.body with
                   []  ->     [print]
                 | [x] ->  x::[print]
                 | x   -> x @ [print]
           )} in
         let ncenv = 
           ( match (snd cenv) with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (String id, ((fst cenv), ncenv)), env  

 | (Ast.Set(id, pty, args), cenv) ->
         let lfdecl = List.hd (List.rev (snd cenv)) in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals; body= (
               let print = 
		(match (pty, args) with
			  ("Contain", [Ast.Var wid]) -> 
		Cast.Expr (Call("gtk_container_add",
	[Cast.Call("GTK_CONTAINER",[Cast.Id id]); Cast.Id wid]))
			| ("FixPos", [Ast.Var wid; Lit(x); Lit(y)]) -> 
		Cast.Expr (Call("gtk_fixed_put",
	[Cast.Call("GTK_FIXED",[Cast.Id wid]); Cast.Id id; Literal x; Literal y]))
			| ("BorderWidth", [Lit(w)]) -> 
		Cast.Expr (Call("gtk_container_set_border_width",
	[Cast.Call("GTK_CONTAINER",[Cast.Id id]); Cast.Literal w]))
      			| ("Title", [Ast.StrLit s])-> 
		Cast.Expr (Call("gtk_window_set_title",
	[Cast.Call("GTK_WINDOW",[Cast.Id id]); StrLit ("\"" ^ s ^ "\"")]))
			| ("WinPosition", [Ast.Var "center"]) -> 
		Cast.Expr (Call("gtk_window_set_position",
	[Cast.Call("GTK_WINDOW",[Cast.Id id]); StrLit("GTK_WIN_POS_CENTER")]))
			| ("WinMove", [Lit(x); Lit(y)]) -> 
		Cast.Expr (Call("gtk_window_move",
	[Cast.Call("GTK_WINDOW",[Cast.Id id]); Cast.Literal x; Cast.Literal y ]))
			| ("WinSize", [Lit(w); Lit(h)]) -> 
		Cast.Expr (Call("gtk_window_set_default_size",
	[Cast.Call("GTK_WINDOW",[Cast.Id id]); Cast.Literal w; Cast.Literal h]))
			| ("Resizable", [Ast.Var "no"]) ->
		Cast.Expr (Call("gtk_window_set_resizable",
	[Cast.Call("GTK_WINDOW",[Cast.Id id]); StrLit("FALSE")]))
			| ("Resizable", [Ast.Var "yes"]) ->
		Cast.Expr (Call("gtk_window_set_resizable",
	[Cast.Call("GTK_WINDOW",[Cast.Id id]); StrLit("TRUE")]))
			| ("FrameLabel",[Ast.StrLit s]) ->
		(Cast.Expr (Call("gtk_frame_set_label",
	[Cast.Call("GTK_FRAME",[Cast.Id id]); StrLit ("\"" ^ s ^ "\"")])))
			| ("FrameShadow", [Ast.Var s]) ->
		(match s with
		  "in" -> Cast.Expr (Call("gtk_frame_set_shadow_type",
	[Cast.Call("GTK_FRAME",[Cast.Id id]); StrLit ("GTK_SHADOW_IN")]))
		| "out" -> Cast.Expr (Call("gtk_frame_set_shadow_type",
	[Cast.Call("GTK_FRAME",[Cast.Id id]); StrLit ("GTK_SHADOW_OUT")]))
		| "etchedin" -> Cast.Expr (Call("gtk_frame_set_shadow_type",
	[Cast.Call("GTK_FRAME",[Cast.Id id]); StrLit ("GTK_SHADOW_ETCHED_IN")]))
		| "etchedout" -> Cast.Expr (Call("gtk_frame_set_shadow_type",
	[Cast.Call("GTK_FRAME",[Cast.Id id]); StrLit ("GTK_SHADOW_ETCHED_OUT")]))
		| _ -> raise (Failure ("Error: Shadow " ^ s ^ " is invalid.")))
			| ("GridAttach", [Ast.Var wid; Lit(x); Lit(y); Lit(w); Lit(h)]) ->
		Cast.Expr (Call("gtk_grid_attach",
	[Cast.Call("GTK_GRID",[Cast.Id id]); Cast.Id wid; Cast.Literal x; 		Cast.Literal y; Cast.Literal w; Cast.Literal h]))
			| ("InsertRow", [Lit(r)]) ->
		Cast.Expr (Call("gtk_grid_insert_row",
	[Cast.Call("GTK_GRID",[Cast.Id id]); Cast.Literal r]))
			| ("InsertColumn", [Lit(c)]) ->
		Cast.Expr (Call("gtk_grid_insert_column",
	[Cast.Call("GTK_GRID",[Cast.Id id]); Cast.Literal c]))
			| ("MenuItemLabel", [Ast.StrLit s]) -> 
		(Cast.Expr (Call("gtk_menu_item_set_label",
	[Cast.Call("GTK_MENU_ITEM",[Cast.Id id]); StrLit ("\"" ^ s ^ "\"")])))
			| ("InMenu", [Ast.Var wid]) ->
		Cast.Expr (Call("gtk_menu_item_set_submenu",
	[Cast.Call("GTK_MENU_ITEM",[Cast.Id id]); Cast.Id wid]))
			| ("AppendMenu", [Ast.Var wid]) ->
		Cast.Expr (Call("gtk_menu_shell_append",
	[Cast.Call("GTK_MENU_SHELL",[Cast.Id wid]); Cast.Id id]))
			| ("BoxPack", [Ast.Var wid]) ->
		Cast.Expr (Call("gtk_box_pack_start",
	[Cast.Call("GTK_BOX",[Cast.Id id]); Cast.Id wid; StrLit("FALSE"); StrLit("FALSE"); Literal 10]))
			| ("ClickQuit", []) ->
		Cast.Expr (Call("g_signal_connect",
	[Cast.Call("G_OBJECT",[Cast.Id id]); StrLit ("\"activate\""); Cast.Call("G_CALLBACK",[StrLit("gtk_main_quit")]); Cast.Null]))
			| (_, _) -> raise (Failure ("Error: Property " ^ pty ^ " is invalid or cannot be changed."))
)
	and show = (Cast.Expr(Call("gtk_widget_show_all",[Cast.Id id]))) in
                 match lfdecl.body with
                   []  ->     print::[show]
                 | [x] ->  x::print::[show]
                 | x   -> x @ [print] @ [show]
           )} in
         let ncenv = 
           ( match (snd cenv) with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (String id, ((fst cenv), ncenv)), env  

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
         | String(x1), String(x2) ->
            String (match op with
                   Ast.Add -> x1 ^ x2
                 | _ -> raise (Failure
                     ("Error: Invalid string operation.")))
         | _ -> raise (Failure
                     ("Error: Invalid operation."))
        ), cenv), vars

let cenv = {
   returnType = BasicType(Cast.Int);
   fname = "main";
   formals = [Cast.FormalDecl(BasicType(Cast.Int),"argc")] @ [FormalDecl(PointerToPointerType(Cast.Char), "argv")];
   locals = [];
   body = [];
}
(*
 | Ast.Act(v, a) ->
         (match (StringMap.find v env) with
			String "Window" -> 
				(match a with
				"destroy" -> (Cast.Call("g_signal_connect", [Cast.Id v; Cast.StrLit a; Cast.Call("G_CALLBACK", [Cast.StrLit "gtk_main_quit"]); Cast.Null]))))	
*)
(*
 | (Ast.Act(v,a), cenv) -> (match (StringMap.find v env) with
			"Window" -> 
				(match Cast.StrLit a with
				"destroy" -> Cast.Expr (Call("g_signal_connect", [Cast.Id v; Cast.StrLit a; Cast.Expr (Call("G_CALLBACK", [Cast.StrLit "gtk_main_quit"])); Cast.Null])))) in (v,cenv), env
*)

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


  let lfdecl = List.hd (List.rev cenv) in
  let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; locals =
    lfdecl.locals; body = (
      (* Append "gtk_init(&argc, &argv);" as first, "return 0;" as the last body declaration *)
      let main_return = Cast.Return(Cast.Literal 0) and gtkinit = Cast.Expr(Call("gtk_init", [Cast.StrLit "&argc"; Cast.StrLit "&argv"])) and gtkmain = Cast.Expr(Call("gtk_main",[])) in
      match lfdecl.body with
        []  -> gtkinit::gtkmain::[main_return]
      | [x] -> gtkinit::x::gtkmain::[main_return]
      | x   -> [gtkinit] @ x @ [gtkmain] @ [main_return] 
      ) } in
  (* Append to the new cenv *)
  let ncenv = 
    ( match cenv with
        []  ->     []
      | [x] ->     [nfdecl]
      | x   -> x @ [nfdecl]) in  
  let listing = Cast.string_of_program (Cast.Program([], cvars, ncenv)) in
  let oc = open_out "prog.c" in 
 fprintf oc "%s\n"  (*printf "%s\n"*) (* Append preprocessor *)
                    ( "#include <stdio.h>\n" ^
                      "#include <gtk/gtk.h>\n" ^ listing  )                 
                      
