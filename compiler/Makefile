.PHONY: default
default:
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.mli
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c pear.ml
	ocamlc -o pear unix.cma parser.cmo scanner.cmo pear.cmo
