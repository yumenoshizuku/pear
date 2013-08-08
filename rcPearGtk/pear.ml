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
   
let getGtkTypeOrFail variableName env= 
   if not (StringMap.mem variableName env) then
 	  raise (Failure ("Error: undefined variable " ^ variableName))
   else
      string_of_primitive (StringMap.find variableName env)		      

(* place main at the last function *)
let cMain = {
   returnType = Cast.BasicType (Cast.Int);
   fname = "main";
   formals = [Cast.FormalDecl (Cast.BasicType (Cast.Int), "argc");
   Cast.FormalDecl (Cast.PointerToPointerType (Cast.Char), "argv")];
   locals = [];
   body = [];
}   

let cenv= {types=[]; globals=[]; funs=[cMain] } 

(* convert a expression list to statement list by creating Cast.Expr for each expression *)
let rec getCStmtListFromExprList = function
   []  -> []
   | hd::tl -> Cast.Expr hd :: getCStmtListFromExprList tl

let rec append_to_list toAppend= function
    []  ->     toAppend
  | [x] ->  x::toAppend
  | x   -> x @ toAppend  

let gtkNameOfType vType = match vType with
     "Label" -> "GTK_LABEL"
   | _ -> raise (Failure ("Error: unknown gtk type " ^ vType))   

let stirng_of_getPtyExpr variable methodType = 
   variable ^ methodType
 

(* arguments: variable, property type, env, cFuns *)
(* returns: return type ("Text", "Numeric", or widget type), C expr list, newCEnv *)
(* if the getter is get a widget in a container, we define a function to get the widget, 
and the C expr list calls this function *)
let getPtyCExpr variable methodType env cenv= 
   let vType = getGtkTypeOrFail variable env in
    match methodType with 
    "getText" -> 
      let functionName = match vType with    
         "Label" -> "gtk_label_get_text"
        | _ -> raise (Failure ("Error: can't get text of type " ^ vType))  
      in
      let exprs = [ Cast.Call (functionName, 
                        [Cast.Call (gtkNameOfType vType, [Cast.Id (variable)])
                        ])]
      in "Text", exprs, cenv
    | _ as unknown -> raise (Failure ("Error: unknown get property method " ^ unknown))
 
let setPtyCExpr variable methodType valueList env cenv= 
  let vType = getGtkTypeOrFail variable env in
  match methodType with
  "setText" -> 
     if not (List.length valueList = 1) then
        raise (Failure ("Error: method setText takes 1 argument"))
     else 
     let argCExprList, cenv =
      match List.hd valueList with 
          Ast.StrLit (x) -> [Cast.StringLit x], cenv
        | Ast.GetPty (v2, ptyType) ->
           let retType, exprList, cenv = getPtyCExpr v2 ptyType env cenv in
           if not (retType = "Text") then
              raise (Failure ("Error: Text argument expected for setText method, actual: " ^ retType))
           else exprList, cenv
        | _ -> raise (Failure ("Error: text argument expected for setText method "))
      in    
      let functionName = match vType with    
         "Label" -> "gtk_label_set_text"
        | _ -> raise (Failure ("Error: can't set text of type " ^ vType)) 
      in        
      let exprList = [Cast.Call (functionName, 
                         Cast.Call (gtkNameOfType vType, [Cast.Id (variable)]) :: argCExprList
                         )]
      in exprList, cenv
   | _ as unknown -> raise (Failure ("Error: unknown set property method " ^ unknown))  

(* create expression for common argument list for creating a widget: initial text, initial x postion, initial y position. Returns three expression lists *)
let getCExprOfTextAndInitPosition argsList env cenv= 
    let arg1CExprList =
      match List.hd argsList with 
          Ast.StrLit (x) -> [Cast.StringLit x]
        | Ast.GetPty (v2, ptyType) ->
            let retType, exprList, cenv= getPtyCExpr v2 ptyType env cenv in
            if not (retType = "Text") then
               raise (Failure ("Error in create widget argument"))
            else exprList
        | _ -> raise (Failure ("Error in create widget argument"))
    in  
    let arg2CExprList =
      match List.hd (List.tl argsList) with 
          Ast.Lit (x) -> [Cast.Literal x]
        | Ast.GetPty (v2, ptyType) ->
            let retType, exprList, cenv = getPtyCExpr v2 ptyType env cenv in
            if not (retType = "Numeric") then
               raise (Failure ("Error in create widget argument"))
            else exprList
        | _ -> raise (Failure ("Error in create widget argument"))
    in 
    let arg3CExprList =
      match List.hd (List.tl (List.tl argsList)) with 
          Ast.Lit (x) -> [Cast.Literal x]
        | Ast.GetPty (v2, ptyType) ->
            let retType, exprList, cenv = getPtyCExpr v2 ptyType env cenv in
            if not (retType = "Numeric") then
               raise (Failure ("Error in create widget argument"))
            else exprList
        | _ -> raise (Failure ("Error in create widget argument"))
    in arg1CExprList, arg2CExprList, arg3CExprList, cenv           

(* returns variable declarations, statements, new env *)
let createCSyntax variable containerVar widgetType valueList env =
  if StringMap.mem variable env then
 		 raise (Failure ("Error: Duplicate variable " ^ variable))
  else		 
  let containerType = getGtkTypeOrFail containerVar env in
  match widgetType with
  "Label" -> 
     if not (containerType = "Window") then
 		 raise (Failure ("Error: Label is not allowed to be created in: " ^ containerType))  
     else if not (List.length valueList = 3) then
        raise (Failure ("Error: Label initialization takes 3 argument (text, initial x position, initial y position)"))    
 	 else                
       let vdeclExpr = [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), variable) ]
       in
       let arg1, arg2, arg3, cenv = getCExprOfTextAndInitPosition valueList env cenv
       in        
       let exprList = [ Cast.Expr (Cast.Assign (variable, (Cast.Call ("gtk_label_new", arg1)))) ;
                   Cast.Expr (Cast.Call ("gtk_fixed_put", [Cast.Call ("GTK_FIXED", [Cast.Id (getFrameNameFromWindowName containerVar)]); Cast.Id(variable)] @ arg2  @ arg3))
                 ]
       in vdeclExpr, exprList, (StringMap.add variable (String "Label") env)
   | "Button" ->
     if not (containerType = "Window") then
 		 raise (Failure ("Error: Button is not allowed to be created in: " ^ containerType))  
     else if not (List.length valueList = 3) then
        raise (Failure ("Error: Button initialization takes 3 argument (text, initial x position, initial y position)"))    
 	 else                
       let vdeclExpr = [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), variable) ]
       in
       let arg1, arg2, arg3, cenv = getCExprOfTextAndInitPosition valueList env cenv
       in        
       let exprList = [ Cast.Expr (Cast.Assign (variable, (Cast.Call ("gtk_button_new_with_label", arg1)))) ;
                   Cast.Expr (Cast.Call ("gtk_fixed_put", [Cast.Call ("GTK_FIXED", [Cast.Id (getFrameNameFromWindowName containerVar)]); Cast.Id(variable)] @ arg2  @ arg3))
                 ]
       in vdeclExpr, exprList, (StringMap.add variable (String "Button") env)       
   | _ as unknown -> raise (Failure ("Error: unknown widget type " ^ unknown)) 

(* input function name, existing c fun list *)
let rec getCallbackFunRecord funName= function 
  [] -> {
   returnType = Cast.BasicType (Cast.Void);
   fname = funName;
   formals = [Cast.FormalDecl (Cast.PointerType (Cast.GtkWidget), "widget");
   Cast.FormalDecl (Cast.BasicType (Cast.GPointer), "data")];
   locals = [];
   body = [];
   } 
  | hd::tl ->
    if hd.fname = funName then
       let hd ={ returnType = hd.returnType; fname = hd.fname; formals = hd.formals; 
             locals=[] ; body= [] } in
       hd
    else
      getCallbackFunRecord funName tl  

(* input updated function, passed funs in list, remaining funs in list *)      
let rec updateCFuns updatedFun passedFuns =function
    [] -> raise (Failure ("Error: pear.ml has bug in updated C functions "))
  | hd :: tl ->
    if hd.fname = updatedFun.fname then
       passedFuns @ [updatedFun] @ tl
    else 
       updateCFuns updatedFun (passedFuns @ [hd]) tl           

(* return register action c name (e.g. "clicked"), callback c function record. Throw exception if fail *)
let regActionHelper actionType callerType callBackFunName cenv =  
  let regActionCName, callbackFunName =
(*  match actionType with
  "click" -> 
      match callerType with 
        "Button" -> "clicked", callBackFunName ^ "Click"
      | _  as unknown ->  raise (Failure ("Error: can't register 'click' action on type " ^ unknown))
  | _ as unknown -> raise (Failure ("Error: unknown action type " ^ unknown))
*)
  match (actionType, callerType) with
    ("click", "Button") -> "clicked", callBackFunName ^ "Click"
  | (_ as x,_ as y ) ->raise (Failure ("Error: unknown action " ^ x ^ " for type "))
  in
  regActionCName, getCallbackFunRecord callbackFunName cenv.funs  

(* current function to add *)
let rec eval curFun env= function
  (Ast.Seq(e1, e2), cenv) ->
       let (v, nCenv), curFun, nEnv = eval curFun env (e1, cenv) in
       eval curFun nEnv (e2, nCenv)   
 | (Ast.Action (objectExpr, actionType, expr), cenv) ->
(* this intends to handle objectExpr be a expr, not just a variable, but has error now*)
(*    let callerType, callbackFunName, callerCExprs, cenv = match objectExpr with 
       Ast.Var(x) -> 
          let vType = getGtkTypeOrFail x env in
          vType, x, [Cast.Id (x)], cenv
       | Ast.GetPty (variable, ptyType) ->
          let retType, exprList, cenv = getPtyCExpr variable ptyType env cenv in 
          retType, (stirng_of_getPtyExpr variable ptyType), exprList, cenv
       | _ -> raise (Failure ("Error: can't register action for the given caller")) 
*)
let vType = getGtkTypeOrFail objectExpr env in
(* callerType: Button, ... ; callbackFunName: button1; callerCExpr: first argument in g_signal_connect function  *)
let callerType, callbackFunName, callerCExprs = vType, objectExpr, [Cast.Id (objectExpr)]
    in   
    (* regActionCName: "clicked" *)
    let regActionCName, callbackFunRecord = regActionHelper actionType callerType callbackFunName cenv
    in
    let cenv = {types=cenv.types; globals=cenv.globals; funs = callbackFunRecord :: cenv.funs} in
    let (_, cenv), callbackFunRecord, env = eval callbackFunRecord env (expr, cenv) in
    let regStmt= Cast.Expr(Cast.Call ("g_signal_connect", 
                 callerCExprs @ [Cast.StringLit(regActionCName); Cast.Call ("G_CALLBACK", [Cast.Id (callbackFunRecord.fname)]); Cast.Null]))
    in    
    let lfdecl = curFun in
    let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals=lfdecl.locals ; body= append_to_list [regStmt] lfdecl.body } in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
    (String "Action", {types=cenv.types; globals=cenv.globals; funs=nfuns}), nfdecl, env   
  | (Ast.Create (variable, containerVar, widgetType, valueList), cenv) ->
       let vdecls, stmts, newEnv = createCSyntax variable containerVar widgetType valueList env in       
         let lfdecl = curFun in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals=lfdecl.locals ; body= append_to_list stmts lfdecl.body } in
         let nfuns =updateCFuns nfdecl [] cenv.funs in 
         (String variable, {types=cenv.types; globals=append_to_list vdecls cenv.globals; funs=nfuns}), nfdecl, newEnv   
 
 | (Ast.Lit(x), cenv) -> (Int x, cenv), curFun, env
 | (Ast.StrLit(x), cenv) -> (String x, cenv), curFun, env
 | (Ast.Char(x), cenv) -> (Char x, cenv), curFun, env
 | (Ast.Var(x), cenv) ->
         if StringMap.mem x env then
             (StringMap.find x env, cenv), curFun, env
         else raise (Failure ("Error: Undeclared identifier " ^ x))      
 | (Ast.Asn(x, e), cenv) ->
         let (value, cenv), curFun, vars = eval curFun env (e, cenv) in 
             (value, cenv), curFun, (StringMap.add x value vars)
 | (Ast.Puts(e1), cenv) -> 
         let (v1, cenv), curFun, vars = eval curFun env (e1, cenv) in
         let lfdecl = curFun in
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
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
         (v1, {types=cenv.types; globals=cenv.globals; funs=nfuns}), curFun, env 
 | (Ast.Window (id), cenv) ->
 		 if StringMap.mem id env then
 		    raise (Failure ("Error: Duplicate variable " ^ id))
 		 else
         let lfdecl = curFun in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals; body= (
               let print = 
                 [ Cast.Expr (Cast.Call ("gtk_init", [Cast.Unaryop(Cast.Ref, Cast.Id("argc")); Cast.Unaryop(Cast.Ref, Cast.Id("argv"))]));
                   Cast.Expr (Cast.Assign (id, (Cast.Call ("gtk_window_new", [Cast.Id ("GTK_WINDOW_TOPLEVEL")])))) ;
                   Cast.Expr (Cast.Assign (getFrameNameFromWindowName id, (Cast.Call ("gtk_fixed_new", []))));
                   Cast.Expr (Cast.Call ("gtk_container_add", [Cast.Call ("GTK_CONTAINER", [Cast.Id (id)]); Cast.Id (getFrameNameFromWindowName id )]))
                 ] in
                append_to_list print lfdecl.body 
           )} in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
         (String id, {types=cenv.types; globals=(
               let print =
                 [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), id) ; 
                  Cast.VDecl (Cast.PointerType (Cast.GtkWidget), getFrameNameFromWindowName id)] 
				 in
                 append_to_list print cenv.globals
           ); funs=nfuns}), nfdecl, (StringMap.add id (String "Window") env) 
                
 | (Ast.Binop(e1, op, e2), cenv) ->
   let (v1, cenv), curFun, vars = eval curFun env (e1, cenv) in
   let (v2, cenv), curFun, vars = eval curFun env (e2, cenv) in
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
        ), cenv), curFun, vars
   | (Ast.GetPty (variable, ptyType), cenv) ->   
         let _, exprs, cenv = getPtyCExpr variable ptyType env cenv in
         let lfdecl = curFun in
         let stmts = getCStmtListFromExprList exprs in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals ; body= append_to_list stmts lfdecl.body } in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
         (String variable, {types=cenv.types; globals=cenv.globals; funs=nfuns}), nfdecl, env   
    | (Ast.SetPty (variable, ptyType, argList), cenv) -> 
         let exprs, cenv = setPtyCExpr variable ptyType argList env cenv in       
         let lfdecl = curFun in
         let stmts = getCStmtListFromExprList exprs in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals ; body= append_to_list stmts lfdecl.body } in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
         (String variable, {types=cenv.types; globals=cenv.globals; funs=nfuns}), nfdecl, env         


let rec exec env = function
   Ast.Expr(e) ->
       eval (List.hd (List.rev cenv.funs)) env (e, cenv)
 | Ast.Return(e) ->
         let (v, nCenv), cFuns, nEnv = eval (List.hd (List.rev cenv.funs)) env (e, cenv) in
   (v, nCenv), cFuns, nEnv   
   
type action = SwAst | SwCast (*| SwInterpret*)


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.stmt Scanner.token lexbuf in
  let (result, cenv), _, evars = exec vars program in

  (* Append "return 0;" at the end of the main method *) 
  let lfdecl = List.hd (List.rev cenv.funs) in
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
  let nfuns = updateCFuns nfdecl [] cenv.funs in  
  let listing = Cast.string_of_program ( {types=cenv.types; globals=cenv.globals; funs=nfuns} ) in
 (* let oc = open_out "prog.c" in *)
 (* fprintf oc "%s\n" *) printf "%s\n" (* Append preprocessor *)
                    ( "#include <stdio.h>\n" ^
                      "#include <gtk/gtk.h>\n" ^ listing  )                 
                                    
                      
