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

let string_of_primitive primitive = match primitive with
    Int(x) -> string_of_int x
 | String(x) -> x
 | Char(x) -> String.make 1 x

let idSeq=0

let string_of_nextId varId = 
   let varId= varId + 1 in 
   string_of_int varId 

let getFrameNameFromWindowName windowName=
   windowName ^ "Frame"       

let cMain = {
   returnType = Cast.BasicType (Cast.Int);
   fname = "main";
   formals = [Cast.FormalDecl (Cast.BasicType (Cast.Int), "argc");
   Cast.FormalDecl (Cast.PointerToPointerType (Cast.Char), "argv")];
   locals = [];
   body = [];
}   

let cenv= {types=[]; globals=[]; funs=[cMain] } 

let rec append_to_list toAppend= function
    []  ->     toAppend
  | [x] ->  x::toAppend
  | x   -> x @ toAppend

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
         let (v1, cenv), vars = eval env (e1, cenv) in
         let lfdecl = List.hd (cenv.funs) in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; locals =
             lfdecl.locals; body = (
               let print = 
                 ( match v1 with
                     Int(x) -> 
                       Cast.Expr (Call("printf", [Cast.StringLit "\"%d\\n\""; Cast.Literal x]))
                   | String(x) -> 
                       Cast.Expr (Call("printf", [Cast.StringLit "\"%s\\n\""; Cast.StringLit ("\"" ^ x ^ "\"")]))
                   | Char(x) -> 
                       Cast.Expr (Call("printf", [Cast.StringLit "\"%c\\n\""; Cast.StringLit ("'" ^ (String.make 1 x) ^ "'")])) ) in
                 match lfdecl.body with
                   []  ->     [print]
                 | [x] ->  x::[print]
                 | x   -> x @ [print] 
           ) } in
         let nfuns = 
           ( match cenv.funs with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (v1, {types=cenv.types; globals=cenv.globals; funs=nfuns}), env 
 | (Ast.Window (id), cenv) ->
 		 if StringMap.mem id env then
 		    raise (Failure ("Error: Duplicate variable " ^ id))
 		 else
         let lfdecl = List.hd (cenv.funs) in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= (
               let print =
                 [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), id) ; 
                  Cast.VDecl (Cast.PointerType (Cast.GtkWidget), getFrameNameFromWindowName id)] 
				 in
                (* match lfdecl.locals with
                   []  ->     print
                 | [x] ->  x:: print
                 | x   -> x @ print *)
                 append_to_list print lfdecl.locals
           ) ; body= (
               let print = 
                 [ Cast.Expr (Cast.Assign (id, (Cast.Call ("gtk_window_new", [Cast.Id ("GTK_WINDOW_TOPLEVEL")])))) ;
                   Cast.Expr (Cast.Call ("gtk_init", [Cast.Unaryop(Cast.Ref, Cast.Id("argc")); Cast.Unaryop(Cast.Ref, Cast.Id("argv"))]));
                   Cast.Expr (Cast.Assign (getFrameNameFromWindowName id, (Cast.Call ("gtk_fixed_new", []))));
                   Cast.Expr (Cast.Call ("gtk_container_add", [Cast.Call ("GTK_CONTAINER", [Cast.Id (id)]); Cast.Id (getFrameNameFromWindowName id )]))
                 ] in
                append_to_list print lfdecl.body 
           )} in
         let nfuns = 
           ( match cenv.funs with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (String id, {types=cenv.types; globals=cenv.globals; funs=nfuns}), (StringMap.add id (String "Window") env) 
                
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
   | (Ast.CreateLabel (lname, wname, text), cenv) ->
     if StringMap.mem lname env then
 		 raise (Failure ("Error: Duplicate variable " ^ lname))
 	 else if not (StringMap.mem wname env) then
 		 raise (Failure ("Error: undefined Window variable " ^ wname))  
     else if not ((string_of_primitive (StringMap.find wname env)) = "Window") then
 		 raise (Failure ("Error: not a Window variable " ^ wname))      
 	 else
         let lfdecl = List.hd (cenv.funs) in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= (
               let print =
                 [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), lname) ] 
				 in
                 append_to_list print lfdecl.locals
           ) ; body= (
               let print = 
                 [ Cast.Expr (Cast.Assign (lname, (Cast.Call ("gtk_label_new", [Cast.StringLit (text)])))) ;
                   Cast.Expr (Cast.Call ("gtk_fixed_put", [Cast.Call ("GTK_FIXED", [Cast.Id (getFrameNameFromWindowName wname)]); Cast.Id (lname); Cast.Literal(190); Cast.Literal(58)]))
                 ] in
                append_to_list print lfdecl.body 
           )} in
         let nfuns = 
           ( match cenv.funs with
               []  ->     []
             | [x] -> [nfdecl]
             | x   -> x @ [nfdecl]) in 
         (String lname, {types=cenv.types; globals=cenv.globals; funs=nfuns}), (StringMap.add lname (String "Label") env) 
        


let rec exec env = function
   Ast.Expr(e) ->
       eval env (e, cenv)
 | Ast.Return(e) ->
         let v, vars = eval env (e, cenv) in
   v, vars
   
type action = SwAst | SwCast (*| SwInterpret*)


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.stmt Scanner.token lexbuf in
  let (result, cenv), evars = exec vars program in

  (* Append "return 0;" at the end of the main method *) 
  let lfdecl = List.hd (cenv.funs) in
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
  let nfuns = 
    ( match cenv.funs with
        []  ->     []
      | [x] ->     [nfdecl]
      | x   -> x @ [nfdecl]) in  
  let listing = Cast.string_of_program ( {types=cenv.types; globals=cenv.globals; funs=nfuns} ) in
  let oc = open_out "prog.c" in 
 (* fprintf oc "%s\n" *) printf "%s\n" (* Append preprocessor *)
                    ( "#include <stdio.h>\n" ^
                      "#include <gtk/gtk.h>\n" ^ listing  )                 
                                    
                      
