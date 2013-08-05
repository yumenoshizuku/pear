.PHONY: default
default:
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.mli
	ocamlc -c cast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c pearnew.ml
	ocamlc -o print cast.cmo parser.cmo scanner.cmo pearnew.cmo
