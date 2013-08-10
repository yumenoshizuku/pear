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

(* To name the frame associated with window *)   
let getFrameNameFromWindowName windowName=
   windowName ^ "Frame" 

(* find type of variable in symbol table *)   
let getVarTypeOrFail variableName env= 
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
   | "TextEntry" -> "GTK_ENTRY"  
   | "Combo" -> "GTK_COMBO"
   | _ -> raise (Failure ("Error: unknown gtk type or not gtk type" ^ vType))   

let stirng_of_getPtyExpr variable methodType = 
   variable ^ methodType
 
(* every 'eval' function should take curFun, cenv and env as argument and return them, 
 because they all may have changed after the evaluation *)

(* input methodType, caller expr *)
(* return C expr list of get operation (usually one expr) *)
let getPtyHelper curFun cenv env methodType vType callerCExprList=  
  match methodType with 
  "getText" -> 
      let functionName = match vType with    
         "Label" -> "gtk_label_get_text"
       | "TextEntry" -> "gtk_entry_get_text" 
       | _ -> raise (Failure ("Error: can't get text of type " ^ vType))        
     in
      let exprs = [ Cast.Call (functionName, 
                        [Cast.Call (gtkNameOfType vType, callerCExprList)
                        ])]
      in curFun, cenv, env, "Text", exprs
  | "getSelection" -> 
       let exprs =
       match vType with    
         "Combo" -> 
           [Cast.Member (Cast.Call (gtkNameOfType vType, callerCExprList), "entry")]
           (*
           let comboSelectionExpr =
           match List.hd callerCExprList with
             Cast.Id (var) -> [Cast.Member (Cast.Call (gtkNameOfType vType, [Cast.Id(var)]), "entry")]
             | _ -> raise (Failure ("Error: select selection of combo must apply on variable expression "))
           in
           comboSelectionExpr  
           *)
         | _ -> raise (Failure ("Error: can't get selection of type " ^ vType))        
     in
     curFun, cenv, env, "TextEntry", exprs      
  | _ as unknown -> raise (Failure ("Error: unknown get property method " ^ unknown))   

(* arguments: property type, caller expr *)
(* returns: return type ("Text", "Numeric", or widget type), C expr list of the get operation (looks it is always one expr) *)
(* Note that we need update cenv when the getter is get a widget in a container, we define a function to get the widget, 
and the C expr list calls this function *)
let rec getPtyCExpr curFun cenv env methodType = function
     Ast.Var(variable) ->
       let vType = getVarTypeOrFail variable env in
       let callerCExpr = [Cast.Id (variable)] in
       getPtyHelper curFun cenv env methodType vType callerCExpr
   | Ast.GetPty (aCallerExpr, aMethodType) ->
       let curFun, cenv, env, retType, callerCExpr = getPtyCExpr curFun cenv env aMethodType aCallerExpr in
       getPtyHelper curFun cenv env methodType retType callerCExpr
   | _ -> raise (Failure ("Error: unsupported expression type to get property "))   

(* evaluate a Var expression *)
(* input varName *)
(* return variable type, c expr list *)
let evalVar curFun cenv env varName=
   let varType = getVarTypeOrFail varName env
   in curFun, cenv, env, varType, [Cast.Id(varName)]   

(* eval Lit, StringLit or Char *)
(* input: expr to evaluate; returns: return type, c expr list *)
(* seems it never changes curFun, cenv or env, but pass and return them anyway to keep a consistent code manner *)
let evalLitTypeExpr curFun cenv env litExpr = match litExpr with
     Ast.Lit(x) -> curFun, cenv, env, "Numeric", [Cast.Literal(x)] 
   | Ast.StrLit(x) -> curFun, cenv, env, "Text", [Cast.StringLit(x)]
   | Ast.Char(x) -> curFun, cenv, env, "Char", [Cast.CharLit(x)]
   | _ -> raise (Failure ("Error: not a lit type "))   

(* evaulate expression that returns something *)
(* all literal expr, Var, GetPty Binop are valid expression here *)
(* input: expr; return retType, c expr list of this operation *)
let rec evalExprRetValue curFun cenv env =function
     Ast.Var(x) -> evalVar curFun cenv env x
   | Ast.GetPty (callerExpr, methodType) -> getPtyCExpr curFun cenv env methodType callerExpr
   | Ast.Lit(x) as e-> evalLitTypeExpr curFun cenv env e
   | Ast.StrLit(x) as e-> evalLitTypeExpr curFun cenv env e
   | Ast.Char(x) as e-> evalLitTypeExpr curFun cenv env e
   | Ast.Paren(e) ->
         let curFun, cenv, env, retType, cExpr = evalExprRetValue curFun cenv env e in
         curFun, cenv, env, retType, [Cast.Paren(List.hd cExpr)]
   | Ast.Binop (e1, op, e2) ->
     let curFun, cenv, env, e1Type, e1CExpr = evalExprRetValue curFun cenv env e1 in
     let curFun, cenv, env, e2Type, e2CExpr = evalExprRetValue curFun cenv env e2 in
     let retType, castOp=
     match op with
      (* TODO not trival to support + for strings, since C doesn't support it *)
        Ast.Add -> if e1Type = "Numeric" && e2Type = "Numeric" then
                "Numeric", Cast.Add
             else raise (Failure ("Error: Add expression should take numeric type expression "))   
      | Ast.Sub -> if e1Type = "Numeric" && e2Type = "Numeric" then
                "Numeric", Cast.Sub
             else raise (Failure ("Error: Sub expression should take numeric type expression ")) 
      | Ast.Mul -> if e1Type = "Numeric" && e2Type = "Numeric" then
                "Numeric", Cast.Mult
             else raise (Failure ("Error: Mul expression should take numeric type expression "))
      | Ast.Div -> if e1Type = "Numeric" && e2Type = "Numeric" then
                "Numeric", Cast.Div
             else raise (Failure ("Error: Div expression should take numeric type expression "))  
      | Ast.Equal -> if e1Type = e2Type  then
                "Boolean", Cast.Equal
             else raise (Failure ("Error: Equal expression should take same type expressions "))
      | Ast.Neq -> if e1Type = e2Type  then
                "Boolean", Cast.Neq
             else raise (Failure ("Error: Neq expression should take same type expressions "))
      | Ast.Less -> if e1Type = e2Type  then
                "Boolean", Cast.Less
             else raise (Failure ("Error: Less expression should take same type expressions "))
      | Ast.Leq -> if e1Type = e2Type  then
                "Boolean", Cast.Leq
             else raise (Failure ("Error: Leq expression should take same type expressions "))  
      | Ast.Greater -> if e1Type = e2Type  then
                "Boolean", Cast.Greater
             else raise (Failure ("Error: Greater expression should take same type expressions "))
      | Ast.Geq -> if e1Type = e2Type  then
                "Boolean", Cast.Geq
             else raise (Failure ("Error: Geq expression should take same type expressions "))                                                                    
      in
      curFun, cenv, env, retType, [Cast.Binop (List.hd e1CExpr, castOp, List.hd e2CExpr)]                                                    
   | _ -> raise (Failure ("Error: not a type that returns a value "))      


(* input varaible, propert type, initialValueList *) 
(* returns cexpr list of the set operation (always one expr) *)
(* cenv is updated when the initial value expr is getPty expr which updates cenv*)
let setPtyCExpr curFun cenv env variable methodType valueList= 
  let vType = getVarTypeOrFail variable env in
  match methodType with
  "setText" -> 
     if not (List.length valueList = 1) then
        raise (Failure ("Error: method setText takes 1 argument"))
     else 
     let curFun, cenv, env, retType, argCExprList = evalExprRetValue curFun cenv env (List.hd valueList) in
     if not (retType = "Text") then
              raise (Failure ("Error: Text argument expected for setText method, actual: " ^ retType))
     else  
      let functionName = match vType with    
          "Label" -> "gtk_label_set_text"
        | "TextEntry" -> "gtk_entry_set_text" 
        | _ -> raise (Failure ("Error: can't set text of type " ^ vType)) 
      in        
      let exprList = [Cast.Call (functionName, 
                         Cast.Call (gtkNameOfType vType, [Cast.Id (variable)]) :: argCExprList
                         )]
      in curFun, cenv, env, exprList
   | _ as unknown -> raise (Failure ("Error: unknown set property method " ^ unknown))  

(* return a expr list of a string type argument, e.g. StrLit or getPty of string type property *)
let stringTypeArgs curFun cenv env argExpr= 
     let curFun, cenv, env, retType, exprList = evalExprRetValue curFun cenv env argExpr in
     if not (retType = "Text") then
              raise (Failure ("Error: Text argument expected, actual: " ^ retType))
     else 
        curFun, cenv, env, exprList

(* return a expr list of a numeric type argument, e.g. Lit or getPty of numeric type property *)
let numericTypeArgs curFun cenv env argExpr = 
     let curFun, cenv, env, retType, exprList = evalExprRetValue curFun cenv env argExpr in
     if not (retType = "Numeric") then
              raise (Failure ("Error: Numeric argument expected, actual: " ^ retType))
     else 
        curFun, cenv, env, exprList

(* create expression for common argument list for creating a widget: 
initial text, initial x postion, initial y position. Returns three expression lists *)
let getCExprOfTextAndInitPosition curFun cenv env argsList= 
    let curFun, cenv, env, arg1CExprList = stringTypeArgs curFun cenv env (List.hd argsList) 
    in  
    let curFun, cenv, env, arg2CExprList = numericTypeArgs curFun cenv env (List.hd (List.tl argsList))
    in 
    let curFun, cenv, env, arg3CExprList = numericTypeArgs curFun cenv env (List.hd (List.tl (List.tl argsList))) 
    in curFun, cenv, env, arg1CExprList, arg2CExprList, arg3CExprList          

(* add values to a glist *)
(* it is a recusive function. input current expr list, list name, remaining values to evaluate *)
(*return exprlist of the add operation*)
let rec addValues curFun cenv env exprList listName=function
    [] -> curFun, cenv, env, exprList
    | hd::tl ->
       let curFun, cenv, env, evalValueExprList=
       match hd with
         Ast.StrLit (x) -> curFun, cenv, env, [Cast.StringLit x]
       | Ast.GetPty (v2, ptyType) ->
          let curFun, cenv, env, retType, exprList= getPtyCExpr curFun cenv env ptyType v2  in
          if not (retType = "Text") then
             raise (Failure ("Error value to pus in string glist is not of string type"))
          else curFun, cenv, env, exprList
       | _ -> raise (Failure ("Error value to pus in string glist is not of string type"))  
       in
       let exprList = exprList @
          [Cast.Assign (listName, Cast.Call("g_list_append", [Cast.Id(listName)] @ evalValueExprList))]
       in   
       addValues curFun cenv env exprList listName tl

(* create a glist with the given entry and update curFun, env and cenv*)
(* input: values to add , glist variable name *)
(* return: vdecl list,  exprList*)
(* currently, add the glist variable to curFun.locals, and env. *)
let createStrGListInCurFun curFun cenv env values listName =
   let vdecl= Cast.VDecl (Cast.PointerType (Cast.GList), listName)
   in
   let exprList = [Cast.Assign (listName, Cast.Null)]
   in
   let curFun, cenv, env, exprList= addValues curFun cenv env exprList listName values 
   in          
   curFun, cenv, env, vdecl, exprList  

(* create expression for common argument list for creating a widget: 
initial x postion, initial y position. Returns two expression lists *)
let getCExprOfInitPosition curFun cenv env argsList = 
    let curFun, cenv, env, arg1CExprList = numericTypeArgs curFun cenv env (List.hd argsList) 
    in  
    let curFun, cenv, env, arg2CExprList = numericTypeArgs curFun cenv env (List.hd (List.tl argsList))
    in curFun, cenv, env, arg1CExprList, arg2CExprList   

(* returns variable declarations, expr list of the create opertion (usually has more than 1 item, e.g. create widget, fix put) *)
(* env will always updates *)
let createCSyntax curFun cenv env variable containerVar widgetType valueList=
  if StringMap.mem variable env then
 		 raise (Failure ("Error: Duplicate variable " ^ variable))
  else		 
  let containerType = getVarTypeOrFail containerVar env in
  match widgetType with
  "Label" -> 
     if not (containerType = "Window") then
 		 raise (Failure ("Error: Label is not allowed to be created in: " ^ containerType))  
     else if not (List.length valueList = 3) then
        raise (Failure ("Error: Label initialization takes 3 argument (text, initial x position, initial y position)"))    
 	 else                
       let vdeclExpr = [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), variable) ]
       in
       let curFun, cenv, env, arg1, arg2, arg3 = getCExprOfTextAndInitPosition curFun cenv env valueList
       in        
       let exprList = [ Cast.Expr (Cast.Assign (variable, (Cast.Call ("gtk_label_new", arg1)))) ;
                   Cast.Expr (Cast.Call ("gtk_fixed_put", [Cast.Call ("GTK_FIXED", [Cast.Id (getFrameNameFromWindowName containerVar)]); Cast.Id(variable)] @ arg2  @ arg3))
                 ]
       in curFun, cenv, (StringMap.add variable (String "Label") env), vdeclExpr, exprList
   | "Button" ->
     if not (containerType = "Window") then
 		 raise (Failure ("Error: Button is not allowed to be created in: " ^ containerType))  
     else if not (List.length valueList = 3) then
        raise (Failure ("Error: Button initialization takes 3 argument (text, initial x position, initial y position)"))    
 	 else                
       let vdeclExpr = [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), variable) ]
       in
       let curFun, cenv, env, arg1, arg2, arg3 = getCExprOfTextAndInitPosition curFun cenv env valueList
       in        
       let exprList = [ Cast.Expr (Cast.Assign (variable, (Cast.Call ("gtk_button_new_with_label", arg1)))) ;
                   Cast.Expr (Cast.Call ("gtk_fixed_put", [Cast.Call ("GTK_FIXED", [Cast.Id (getFrameNameFromWindowName containerVar)]); Cast.Id(variable)] @ arg2  @ arg3))
                 ]
       in curFun, cenv, (StringMap.add variable (String "Button") env), vdeclExpr, exprList 
   | "TextEntry" ->
     if not (containerType = "Window") then
 		 raise (Failure ("Error: TextEntry is not allowed to be created in: " ^ containerType))  
     else if not (List.length valueList = 3) then
        raise (Failure ("Error: TextEntry initialization takes 3 argument (text, initial x position, initial y position)"))    
 	 else                
       let vdeclExpr = [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), variable) ]
       in
       let env = (StringMap.add variable (String "TextEntry") env)  
       in
       let curFun, cenv, env, setTextCExpr= setPtyCExpr curFun cenv env variable "setText" [List.hd valueList]
       in
       let curFun, cenv, env, arg2, arg3 = getCExprOfInitPosition curFun cenv env (List.tl valueList) 
       in  
       let exprList = [ Cast.Expr (Cast.Assign (variable, (Cast.Call ("gtk_entry_new", [])))) ;
                   Cast.Expr (Cast.Call ("gtk_fixed_put", [Cast.Call ("GTK_FIXED", [Cast.Id (getFrameNameFromWindowName containerVar)]); Cast.Id(variable)] @ arg2  @ arg3)) 
                   ] @ getCStmtListFromExprList setTextCExpr
       in curFun, cenv, env, vdeclExpr, exprList 
   | "Combo" ->
       if not (containerType = "Window") then
 		   raise (Failure ("Error: Combo is not allowed to be created in: " ^ containerType))  
       (* argument to initialize combo: initial x, initial y, selection list *)
       else if (List.length valueList < 2) then
          raise (Failure ("Error: Combo initialization takes at least 2 argument (initial x position, initial y position)"))
       else    
          let vdeclExpr = [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), variable) ]
          in 
          let curFun, cenv, env, arg1, arg2 = getCExprOfInitPosition curFun cenv env [List.hd valueList; List.hd (List.tl valueList)]
          in
          let glistName=variable ^ "InitGList"
          in
          let curFun, cenv, env, glistDecl, createGListExpr= 
              createStrGListInCurFun curFun cenv env (List.tl (List.tl valueList)) glistName
          in
          let exprList =getCStmtListFromExprList createGListExpr @ 
          [ Cast.Expr (Cast.Assign (variable, (Cast.Call ("gtk_combo_new", [])))) ;
                   Cast.Expr (Cast.Call ("gtk_fixed_put", [Cast.Call ("GTK_FIXED", [Cast.Id (getFrameNameFromWindowName containerVar)]); Cast.Id(variable)] @ arg1  @ arg2)) ; 
                   Cast.Expr (Cast.Call ("gtk_combo_set_popdown_strings", [Cast.Call (gtkNameOfType "Combo", [Cast.Id(variable)]); Cast.Id(glistName)]))
                   ]
          in curFun, cenv, (StringMap.add variable (String "Combo") env) , vdeclExpr @ [glistDecl], exprList                       	                     
   | _ as unknown -> raise (Failure ("Error: unknown widget type " ^ unknown)) 

(* create a new C fun record according to the given name. if there is already a function with the same name, 
create a new one *)
(* input: function name to match, longest matched function name, existing c fun list *)
(* return: the function record *)
let rec getCallbackFunRecord funName longestMatchedItem= function 
  [] -> 
  let finalName= if longestMatchedItem = "" then funName
                 else longestMatchedItem ^ "New" in
   {
   returnType = Cast.BasicType (Cast.Void);
   fname = finalName;
   formals = [Cast.FormalDecl (Cast.PointerType (Cast.GtkWidget), "widget");
   Cast.FormalDecl (Cast.BasicType (Cast.GPointer), "data")];
   locals = [];
   body = [];
   } 
  | hd::tl ->
    let longestMatchedItem =
    if String.length hd.fname >= String.length funName && String.sub hd.fname 0 (String.length funName) = funName then
       if String.length hd.fname >= String.length longestMatchedItem then
          hd.fname
       else
          longestMatchedItem   
    else
      longestMatchedItem
    in  
      getCallbackFunRecord funName longestMatchedItem tl  

(* input: updated function, passed funs in list, remaining funs in list *)      
(* return: updated function list *)
let rec updateCFuns updatedFun passedFuns =function
    [] -> raise (Failure ("Error: pear.ml has bug in updated C functions "))
  | hd :: tl ->
    if hd.fname = updatedFun.fname then
       passedFuns @ [updatedFun] @ tl
    else 
       updateCFuns updatedFun (passedFuns @ [hd]) tl           

(* *)
(* return: register action c name (e.g. "clicked"), callback c function record. Throw exception if fail *)
let regActionHelper cenv actionType callerType callBackFunName =  
  let regActionCName, callbackFunName =
  match (actionType, callerType) with
    ("click", "Button") -> "clicked", callBackFunName ^ "Click"
  | (_ as x,_ as y ) ->raise (Failure ("Error: unknown action " ^ x ^ " for type "))
  in
  regActionCName, getCallbackFunRecord callbackFunName "" cenv.funs  

(* eval Show expr *)
(* returns updated curFun, updated cenv*)
let evalShow curFun cenv env variable = 
  let vType = getVarTypeOrFail variable env in
  let stmts =
  match vType with 
    "Window" -> [Cast.Expr(Cast.Call("gtk_widget_show_all", [Cast.Id(variable)]))]
    | _ as unknown -> raise (Failure ("Error: can't apply 'show' on type " ^ unknown ))
  in
  let nfdecl = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
             locals=curFun.locals ; body= append_to_list stmts curFun.body } in
  let nfuns =updateCFuns nfdecl [] cenv.funs in 
  nfdecl, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env    
   
(* input curFun cenv env expr to evaluate*)
(* return curFun cenv env return type desc *)
let rec eval curFun cenv env= function
  Ast.Seq(e1, e2) ->
       let curFun, cenv, env, v = eval curFun cenv env e1 in
       eval curFun cenv env e2
(* only allow this expression for e that is legal for evalExprRetValue *)       
 | Ast.Paren (e) as exp->
      let curFun, cenv, env, retType, cExpr = evalExprRetValue curFun cenv env exp in
      let stmt =Cast.Expr(List.hd cExpr)
      in
      let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
             locals=curFun.locals ; body= curFun.body @ [stmt] } in
      let nfuns =updateCFuns curFun [] cenv.funs in 
      curFun, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String retType              
 | Ast.Var(x) ->
      let curFun, cenv, env, varType, cExpr = evalVar curFun cenv env x in
      let stmts=getCStmtListFromExprList cExpr in
      let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
             locals=curFun.locals ; body= append_to_list stmts curFun.body } in
      let nfuns =updateCFuns curFun [] cenv.funs in 
      curFun, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String varType 
(*define non-Gtk variable*)
 | Ast.Asn(x, e)->
      if StringMap.mem x env then
 	      raise (Failure ("Error: duplicate variable " ^ x))
 	  else 
 	     let curFun, cenv, env, retType, cExpr = evalExprRetValue curFun cenv env e in
 	     let vdecl = match retType with
               "Char" -> Cast.VDecl(Cast.BasicType(Cast.Char), x)
             | "Numeric" -> Cast.VDecl(Cast.BasicType(Cast.Int), x)
             | "Text" -> Cast.OneDArrDecl(Cast.BasicType(Cast.Char), x, Cast.Literal(256))
             | _ as unknown -> raise (Failure ("Error: unsupport primitive variable " ^ unknown))
         in    
 	     let stmts = match retType with
               "Char" -> [Cast.Expr (Cast.Assign(x, List.hd cExpr)) ]
             | "Numeric" -> [Cast.Expr (Cast.Assign(x, List.hd cExpr)) ]
             | "Text" -> [Cast.Expr (Cast.Call("strcpy", [Cast.Id(x); List.hd cExpr])) ]
             | _ as unknown -> raise (Failure ("Error: unsupport primitive variable " ^ unknown))
 	     in
 	     let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
             locals=curFun.locals; body= append_to_list stmts curFun.body } in
         let nfuns =updateCFuns curFun [] cenv.funs in 
         curFun, {types=cenv.types; globals=cenv.globals @ [vdecl]; funs=nfuns}, (StringMap.add x (String retType) env), String retType    

 | Ast.Puts(e) -> 
       let curFun, cenv, env, retType, cExpr = evalExprRetValue curFun cenv env e in
       let formatDef= match retType with
            "Char" -> Cast.StringLit("%c\\n")
          | "Numeric" -> Cast.StringLit("%d\\n")
          | "Text" -> Cast.StringLit("%s\\n")
          | _ as unknown -> raise (Failure ("Error: unsupport puts expression type " ^ unknown))
       in
       let stmt = Cast.Expr(Cast.Call("printf", [formatDef] @ cExpr)) in 
 	   let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
             locals=curFun.locals; body= curFun.body @ [stmt]} in
       let nfuns =updateCFuns curFun [] cenv.funs in 
       curFun, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String "Puts"       
 | Ast.Binop(e1, op, e2)  as e->
      let curFun, cenv, env, retType, cExpr = evalExprRetValue curFun cenv env e in
      let stmts=getCStmtListFromExprList cExpr in
      let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
             locals=curFun.locals ; body= append_to_list stmts curFun.body } in
      let nfuns =updateCFuns curFun [] cenv.funs in 
      curFun, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String retType   
                     
 | Ast.GtkMain ->
   let stmts = [Cast.Expr(Cast.Call("gtk_main", []))] in
   let nfdecl = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
             locals=curFun.locals ; body= append_to_list stmts curFun.body } in
   let nfuns =updateCFuns nfdecl [] cenv.funs in 
   nfdecl, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String "GtkMain"     
 | Ast.Show (variable) ->
   let curFun, cenv, env= evalShow curFun cenv env variable in
    curFun, cenv, env, String "Show"     
 | Ast.Action (callerExpr, actionType, expr) ->
(* below code intends to handle callerExpr be a expr, not just a variable, but has error now*)
(*    let callerType, callbackFunName, callerCExprs, cenv = match callerExpr with 
       Ast.Var(x) -> 
          let vType = getVarTypeOrFail x env in
          vType, x, [Cast.Id (x)], cenv
       | Ast.GetPty (variable, ptyType) ->
          let retType, exprList, cenv = getPtyCExpr variable ptyType env cenv in 
          retType, (stirng_of_getPtyExpr variable ptyType), exprList, cenv
       | _ -> raise (Failure ("Error: can't register action for the given caller")) 
*)
let vType = getVarTypeOrFail callerExpr env in
(* callerType: Button, ... ; callbackFunName: button1; callerCExpr: first argument in g_signal_connect function  *)
let callerType, callbackFunName, callerCExprs = vType, callerExpr, [Cast.Id (callerExpr)]
    in   
    (* regActionCName: "clicked" *)
    let regActionCName, callbackFunRecord = regActionHelper cenv actionType callerType callbackFunName
    in
    (* add the new function record to cenv *)
    let cenv = {types=cenv.types; globals=cenv.globals; funs = callbackFunRecord :: cenv.funs} in
    (* eval action 'function' *)
    let callbackFunRecord, cenv, env, _ = eval callbackFunRecord cenv env expr in
    let regStmt= Cast.Expr(Cast.Call ("g_signal_connect", 
                 callerCExprs @ [Cast.StringLit(regActionCName); Cast.Call ("G_CALLBACK", [Cast.Id (callbackFunRecord.fname)]); Cast.Null]))
    in    
    let lfdecl = curFun in
    let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals=lfdecl.locals ; body= append_to_list [regStmt] lfdecl.body } in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
    nfdecl, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String "Action"
  
  | Ast.Create (variable, containerVar, widgetType, valueList) ->
       let curFun, cenv, env, vdecls, stmts= createCSyntax curFun cenv env variable containerVar widgetType valueList in       
         let lfdecl = curFun in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals=lfdecl.locals ; body= append_to_list stmts lfdecl.body } in
         let nfuns =updateCFuns nfdecl [] cenv.funs in 
         nfdecl, {types=cenv.types; globals=append_to_list vdecls cenv.globals; funs=nfuns}, env, String variable   
 
 | Ast.Lit(x) as e -> 
   let curFun, cenv, env, retType, cExpr =evalLitTypeExpr curFun cenv env e
   in
   let stmts=getCStmtListFromExprList cExpr in
   let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
          locals=curFun.locals ; body= append_to_list stmts curFun.body } in
   let nfuns =updateCFuns curFun [] cenv.funs in 
   curFun, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String retType     
 | Ast.StrLit(x) as e-> 
   let curFun, cenv, env, retType, cExpr =evalLitTypeExpr curFun cenv env e
   in
   let stmts=getCStmtListFromExprList cExpr in
   let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
          locals=curFun.locals ; body= append_to_list stmts curFun.body } in
   let nfuns =updateCFuns curFun [] cenv.funs in 
   curFun, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String retType  
 | Ast.Char(x) as e -> 
   let curFun, cenv, env, retType, cExpr =evalLitTypeExpr curFun cenv env e
   in
   let stmts=getCStmtListFromExprList cExpr in
   let curFun = { returnType = curFun.returnType; fname = curFun.fname; formals = curFun.formals; 
          locals=curFun.locals ; body= append_to_list stmts curFun.body } in
   let nfuns =updateCFuns curFun [] cenv.funs in 
   curFun, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String retType   

 | Ast.Window (id) ->
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
                   Cast.Expr (Cast.Call ("gtk_container_add", [Cast.Call ("GTK_CONTAINER", [Cast.Id (id)]); Cast.Id (getFrameNameFromWindowName id )]));
                   Cast.Expr (Cast.Call ("g_signal_connect", [Cast.Id(id); Cast.StringLit("destroy"); Cast.Call("G_CALLBACK", [Cast.Id("gtk_main_quit")]); Cast.Null]))
                 ] in
                append_to_list print lfdecl.body 
           )} in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
         nfdecl, {types=cenv.types; globals=(
               let print =
                 [Cast.VDecl (Cast.PointerType (Cast.GtkWidget), id) ; 
                  Cast.VDecl (Cast.PointerType (Cast.GtkWidget), getFrameNameFromWindowName id)] 
				 in
                 append_to_list print cenv.globals
           ); funs=nfuns}, (StringMap.add id (String "Window") env), (String id) 
                
 (* get properties of widget *)
   | Ast.GetPty (callerExpr, ptyType)->   
         let curFun, cenv, env, retType, exprs = getPtyCExpr curFun cenv env ptyType callerExpr in
         let lfdecl = curFun in
         let stmts = getCStmtListFromExprList exprs in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals ; body= append_to_list stmts lfdecl.body } in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
         nfdecl, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String retType
 
    | Ast.SetPty (variable, ptyType, argList)-> 
         let curFun, cenv, env, exprs = setPtyCExpr curFun cenv env variable ptyType argList in       
         let lfdecl = curFun in
         let stmts = getCStmtListFromExprList exprs in
         let nfdecl = { returnType = lfdecl.returnType; fname = lfdecl.fname; formals = lfdecl.formals; 
             locals= lfdecl.locals ; body= append_to_list stmts lfdecl.body } in
         let nfuns = updateCFuns nfdecl [] cenv.funs in 
         nfdecl, {types=cenv.types; globals=cenv.globals; funs=nfuns}, env, String variable     


let rec exec env = function
   Ast.Expr(e) ->
       eval (List.hd (List.rev cenv.funs)) cenv env e
 | Ast.Return(e) ->
         let curFun, cenv, env, v = eval (List.hd (List.rev cenv.funs)) cenv env e in
   curFun, cenv, env, v         
   
type action = SwAst | SwCast (*| SwInterpret*)


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.stmt Scanner.token lexbuf in
  let _, cenv, evars, result = exec vars program in

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
                      "#include <gtk/gtk.h>\n" ^ 
                      "#include <string.h>\n" ^ listing  )                 
                                    
                      
