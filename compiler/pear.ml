open Ast
open Cast
open Printf

module StringMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
  end)
let vars = StringMap.empty
 
exception ReturnException of string * string StringMap.t

let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t

let rec eval env = function
   (Ast.Lit(x), cenv) -> (string_of_int x, cenv), env
 | (Ast.Var(x), cenv) ->
         if StringMap.mem x env then
             (StringMap.find x env, cenv), env
         else raise (Failure ("Error: Undeclared identifier " ^ x))
 | (Ast.StrLit(x), cenv) -> (x, cenv), env
 | (Ast.Char(x), cenv) -> (String.make 1 x, cenv), env
 | (Ast.Seq(e1, e2), cenv) ->
         let value, vars = eval env (e1, cenv) in
         eval vars (e2, cenv)
 | (Ast.Asn(x, e), cenv) ->
         let (value, cenv), vars = eval env (e, cenv) in 
             (value, cenv), (StringMap.add x value vars)
 | (Ast.Puts(e1), cenv) -> 
         let (v1, (cvars, cenv)), vars = eval env (e1, cenv) in

         let rcenv = List.rev cenv in
         let rcenv = (match rcenv with
               [] -> raise (Failure ("Error: Syntax error."))
             |  [x] -> 
                     ignore (x.body = [Cast.Expr (Call("print", [Cast.Literal
               (int_of_string v1)]))]); [x]
             | h :: _ -> ignore (h.body = [Cast.Expr (Call("print",
             [Cast.Literal
               (int_of_string v1)]))]); [h]) in
         let cenv = List.rev rcenv in
         let head = List.hd cenv in
         let hbody = List.hd head.body in
         print_string (string_of_stmt (hbody));
         (*let (last cenv).body = Call("print", [v1])::cenv.body;*)
         (("printf(\"%s\\n\", " ^ v1 ^ ");"), (cvars, cenv)), env 
 | (Ast.Binop(e1, op, e2), cenv) ->
   let (v1, cenv), vars = eval env (e1, cenv) in
   let (v2, cenv), vars = eval env (e2, cenv) in
       ((match op with
           Ast.Add -> string_of_int ((int_of_string v1) + (int_of_string v2))
         | Ast.Sub -> string_of_int ((int_of_string v1) - (int_of_string v2))
         | Ast.Mul -> string_of_int ((int_of_string v1) * (int_of_string v2))
         | Ast.Div -> string_of_int ((int_of_string v1) / (int_of_string v2))),
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
   
   (*raise (ReturnException(v, vars))*)

let () =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.stmt Scanner.token lexbuf in
    let (result, cenv), evars = exec vars expr in
    let oc = open_out "prog.c" in
    let coc = open_out "cprog.c" in
    (* Wrap main method and libraries *)
    fprintf oc "%s\n" ("#include <stdio.h>\n" ^ 
                       "#include <gtk/gtk.h>\n" ^
                       "int main() {\n" ^ result ^ "\n}");
    let listing = Cast.string_of_program cenv
    in fprintf coc "%s\n" listing



(* Can I make prog.c write to file before running the following?
 *
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
