open Ast
open Printf

module StringMap = Map.Make(struct
  type t = string
  let compare x y = Pervasives.compare x y
  end)
let vars = StringMap.empty;;

  (* Can I make this function return int | string | char?
   * so that I can match op with ...*)
let rec eval env = function
   Lit(x) -> string_of_int x, env
 | Var(x) ->
         if StringMap.mem x env then
             StringMap.find x env, env
         else raise (Failure ("Error: Undeclared identifier " ^ x))
 | StrLit(x) -> x, env
 | Char(x) -> String.make 1 x, env
 | Seq(e1, e2) ->
         let value, vars = eval env e1 in
         eval vars e2;
 | Asn(x, e) ->
         let value, vars = eval env e in 
             value, (StringMap.add x value vars);
 | Puts(e1) -> 
         let v1, vars = eval env e1 in
         (* Printf for puts and char* for string *)
         ("printf(\"%s\\n\", " ^ v1 ^ ");"), env; 
 | Binop(e1, op, e2) ->
   let v1, vars = eval env e1 in
   let v2, vars = eval env e2 in
       (match op with
           Add -> string_of_int ((int_of_string v1) + (int_of_string v2))
         | Sub -> string_of_int ((int_of_string v1) - (int_of_string v2))
         | Mul -> string_of_int ((int_of_string v1) * (int_of_string v2))
         | Div -> string_of_int ((int_of_string v1) / (int_of_string v2))), vars

let () =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result, evars = eval vars expr in
    let oc = open_out "prog.c" in
    (* Wrap main method and libraries *)
    fprintf oc "%s\n" ("#include <stdio.h>\n" ^ 
                       "#include <gtk/gtk.h>\n" ^
                       "int main() {\n" ^ result ^ "\n}")
;;


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
