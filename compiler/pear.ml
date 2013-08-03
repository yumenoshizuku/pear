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
         (*let (value, cenv2), vars = eval env (e1, cenv) in*)
         let (value, ncenv), vars = eval vars (e1, cenv) in
         eval vars (e2, ncenv)
 | (Ast.Asn(x, e), cenv) ->
         let (value, cenv), vars = eval env (e, cenv) in 
             (value, cenv), (StringMap.add x value vars)
 | (Ast.Puts(e1), cenv) -> 
         let (v1, (cvars, cenv)), vars = eval env (e1, cenv) in
        (* print_string (string_of_int (List.length ((List.hd cenv).body))); *)
         let rcenv = List.rev cenv in
         let head = List.hd rcenv in
         let temp = { fname = head.fname; formals = head.formals; locals =
             head.locals; body = (
                 let print = 
                    ( match v1 with
                      Int(x) -> Cast.Expr (Call("printf", [Cast.Id "\"%d\\n\""; 
                        Cast.Literal x]))
                    | String(x) -> Cast.Expr (Call("printf", [Cast.Id "\"%s\\n\""; 
                        Cast.Id ("\"" ^ x ^ "\"")]))
                    | Char(x) -> Cast.Expr (Call("printf", [Cast.Id "\"%c\\n\"";
                        Cast.Id ("'" ^ (String.make 1 x) ^ "'")])) ) in
                 match head.body with
                   [] -> [print]
                 | [x] -> x::[print]
                 | x  -> x @ [print] 
             ) } in
         let ncenv = temp::(List.tl rcenv) in
         let rncenv = List.rev ncenv in
         (v1, (cvars, rncenv)), env 
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
            | _ -> raise (Failure ("Error: Syntax error. Cannot perform op on
            string")))
        | _ -> raise (Failure ("Error: Syntax error. Cannot perform op on
        string.")) 
        ),
         cenv), vars

let cenv = {
   fname = "main";
   formals = [];
   locals = [];
   body = [];
}

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
  let (result, cenv), evars = exec vars program in
  let listing = Cast.string_of_program cenv in
  let oc = open_out "prog.c" in 
  fprintf oc "%s\n" listing
