all:
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c -g ast.mli
	ocamlc -c -g parser.mli
	ocamlc -c -g scanner.ml
	ocamlc -c -g parser.ml
	ocamlc -c -g WhiteSpace.ml
	ocamlc -c -g Inspector.ml
	ocamlc -c -g inspect.ml
	ocamlc -c -g Pretty.ml
	ocamlc -c -g prettify.ml
	ocamlc -c -g streams.ml
	ocamlc -c -g canonical.ml
	ocamlc -g -o tools/prettify scanner.cmo parser.cmo Inspector.cmo Pretty.cmo WhiteSpace.cmo prettify.cmo
	ocamlc -g -o tools/inspect scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo inspect.cmo
	ocamlc -g -o tools/streams scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo streams.cmo
	ocamlc -g -o tools/canonical scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo canonical.cmo

clean:
	rm *.cmi *.cmo parser.ml parser.mli scanner.ml
