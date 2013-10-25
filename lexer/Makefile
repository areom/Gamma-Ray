all:
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c -g ast.mli
	ocamlc -c -g parser.mli
	ocamlc -c -g scanner.ml
	ocamlc -c -g parser.ml
	ocamlc -c inspect.ml
	ocamlc -o inspect scanner.cmo parser.cmo inspect.cmo

clean:
	rm *.cmi *.cmo parser.ml parser.mli scanner.ml
