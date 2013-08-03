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

exception ReturnException of string * string StringMap.t

let rec eval env = function
   (Ast.Lit(x), cenv) -> (Int x, cenv), env
 | (Ast.StrLit(x), cenv) -> (String x, cenv), env
 | (Ast.Char(x), cenv) -> (Char x, cenv), env
 | (Ast.Var(x), cenv) ->
         if StringMap.mem x env then
             (StringMap.find x env, cenv), env
         else raise (Failure ("Error: Undeclared identifier " ^ x))
 | (Ast.Seq(e1, e2), cenv) ->
         let value, vars = eval env (e1, cenv) in
         eval vars (e2, cenv)
 | (Ast.Asn(x, e), cenv) ->
         let (value, cenv), vars = eval env (e, cenv) in 
             (value, cenv), (StringMap.add x value vars)
 | (Ast.Puts(e1), cenv) -> 
         let (v1, (cvars, cenv)), vars = eval env (e1, cenv) in
         print_string (string_of_int (List.length ((List.hd cenv).body)));
         let head = List.hd cenv in
         let temp = { fname = head.fname; formals = head.formals; locals =
             head.locals; body = (
                 match v1 with
                 Int(x) -> [Cast.Expr (Call("print_int", [Cast.Literal x]))]
                 | String(x) -> [Cast.Expr (Call("print_str", [Cast.Id x]))] 
                 | Char(x) -> [Cast.Expr (Call("print_chr", [Cast.Id (String.make 1 x)]))]  
             ) } in
         let cenv = temp::(List.tl cenv) in
         (*
         ((match v1 with
         Int(x) -> String ("printf(\"%d\\n\", " ^ (string_of_int x) ^ ");")
         | String(x) -> String ("printf(\"%s\\n\", " ^ x ^ ");")
         | Char(x) -> String ("printf(\"%s\\n\", " ^ (String.make 1 x) ^ ");")) 
         *)
         (v1, (cvars, cenv)), env 
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
   
type action = SwAst | SwCast | SwInterpret

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-c", SwCast);
                              ("-a", SwAst);
                              ("-i", SwInterpret)]
  else SwCast in 
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.stmt Scanner.token lexbuf in
  let (result, cenv), evars = exec vars program in

  match action with
    SwAst ->
    let oc = open_out "prog.pt" in
    (* Wrap main method and libraries *)
    fprintf oc "%s\n" ("#include <stdio.h>\n" ^ 
                       "#include <gtk/gtk.h>\n" ^
                       "int main() {\n" ^ "result" ^ "\n}");
  | SwCast -> let listing = Cast.string_of_program cenv in
           let oc = open_out "prog.c" in 
           fprintf oc "%s\n" listing
  | SwInterpret -> ignore (Interpret.run cenv)





(*
(* Compile prog.c with gcc.
 * The switches are obtained from the command:
 * `pkg-config --cflags --libs gtk+-3.0` *)
open Unix;;
let gcc () = execvp "gcc" [|"gcc"; "-o"; "prog"; "-pthread";
"-I/usr/include/gtk-3.0"; "-I/usr/include/at-spi2-atk/2.0";
"-I/usr/include/gtk-3.0"; "-I/usr/include/gio-unix-2.0/";
"-I/usr/include/cairo"; "-I/usr/include/pango-1.0"; "-I/usr/include/harfbuzz";
"-I/usr/include/pango-1.0"; "-I/usr/include/atk-1.0"; "-I/usr/include/cairo";
"-I/usr/include/pixman-1"; "-I/usr/include/freetype2"; "-I/usr/include/libdrm";
"-I/usr/include/libpng16"; "-I/usr/include/gdk-pixbuf-2.0";
"-I/usr/include/libpng16"; "-I/usr/include/glib-2.0";
"-I/usr/lib/glib-2.0/include"; "-lgtk-3"; "-lgdk-3"; "-lpangocairo-1.0";
"-lpango-1.0"; "-latk-1.0"; "-lcairo-gobject"; "-lcairo"; "-lgdk_pixbuf-2.0";
"-lgio-2.0"; "-lgobject-2.0"; "-lglib-2.0"; "prog.c"|];;
handle_unix_error gcc ();;
*)
