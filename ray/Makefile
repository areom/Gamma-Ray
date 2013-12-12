all: compile docs tools

compile:
	#Generate the lexer and parser
	ocamllex scanner.mll
	ocamlyacc parser.mly

	#BuildSast up the source compilation
	ocamlc -c -g ast.mli
	ocamlc -c -g parser.mli
	ocamlc -c -g scanner.ml
	ocamlc -c -g parser.ml
	ocamlc -c -g WhiteSpace.ml
	ocamlc -c -g Inspector.ml
	ocamlc -c -g Pretty.ml
	ocamlc -c -g Util.ml
	ocamlc -c -g StringModules.ml
	ocamlc -c -g Klass.ml
	ocamlc -c -g classinfo.ml
	ocamlc -c -g inspect.ml
	ocamlc -c -g prettify.ml
	ocamlc -c -g streams.ml
	ocamlc -c -g canonical.ml
	ocamlc -c -g Debug.ml
	ocamlc -c -g Variables.ml
	ocamlc -c -g freevars.ml
	ocamlc -c -g Sast.ml
	ocamlc -c -g BuildSast.ml
	ocamlc -c -g Unanonymous.ml
	#Make the tools
	ocamlc -g -o tools/prettify scanner.cmo parser.cmo Inspector.cmo Pretty.cmo WhiteSpace.cmo prettify.cmo
	ocamlc -g -o tools/inspect scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo inspect.cmo
	ocamlc -g -o tools/streams scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo streams.cmo
	ocamlc -g -o tools/canonical scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo canonical.cmo
	ocamlc -g -o tools/freevars scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo Util.cmo StringModules.cmo Klass.cmo Debug.cmo Variables.cmo freevars.cmo
	ocamlc -g -o tools/classinfo scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo Util.cmo StringModules.cmo Klass.cmo classinfo.cmo

docsources = ast.mli scanner.ml parser.mli parser.ml WhiteSpace.ml Inspector.ml Pretty.ml Util.ml StringModules.ml Klass.ml Debug.ml Variables.ml Unanonymous.ml
docgen = ./doc/.docgen

docs:
	#Generate the documentation
	mkdir -p doc
	ocamldoc -dump $(docgen) -keep-code $(docsources)
	ocamldoc -load $(docgen) -d doc -t "The Ray Compiler" -html -colorize-code -all-params
	ocamldoc -load $(docgen) -dot -o "./doc/ray-modules.dot"
	ocamldoc -load $(docgen) -dot -dot-types -o "./doc/ray-types.dot"

clean:
	rm *.cmi *.cmo parser.ml parser.mli scanner.ml
	rm -r ./doc

cleantools:
	rm tools/{prettify,inspect,streams,canonical,freevars,classinfo}
