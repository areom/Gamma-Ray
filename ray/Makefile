all:
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c -g ast.mli
	ocamlc -c -g parser.mli
	ocamlc -c -g scanner.ml
	ocamlc -c -g parser.ml
	ocamlc -c inspect.ml
	ocamlc -c streams.ml
	ocamlc -o inspect scanner.cmo parser.cmo inspect.cmo
	ocamlc -o streams scanner.cmo parser.cmo streams.cmo

clean:
	rm *.cmi *.cmo parser.ml parser.mli scanner.ml
