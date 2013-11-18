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
	ocamlc -c -g streams.ml
	ocamlc -c -g unscan.ml
	ocamlc -g -o inspect scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo inspect.cmo
	ocamlc -g -o streams scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo streams.cmo
	ocamlc -g -o unscan scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo unscan.cmo

clean:
	rm *.cmi *.cmo parser.ml parser.mli scanner.ml
