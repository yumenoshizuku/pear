.PHONY: default
default:
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.mli
	ocamlc -c cast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c pear.ml
	ocamlc -o pear unix.cma parser.cmo scanner.cmo cast.cmo pear.cmo
	rm ast.c*
	rm cast.c*
	rm parser.c*
	rm parser.ml
	rm parser.mli
	rm pear.c*
	rm scanner.c*
	rm scanner.ml