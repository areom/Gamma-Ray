all: compile _tools _doc

compile:
	#Generate the lexer and parser
	ocamllex scanner.mll
	ocamlyacc parser.mly

	ocamlc -c -g Ast.mli
	ocamlc -c -g UID.ml

	ocamlc -c -g parser.mli
	ocamlc -c -g scanner.ml
	ocamlc -c -g parser.ml

	ocamlc -c -g WhiteSpace.ml
	ocamlc -c -g Inspector.mli
	ocamlc -c -g Inspector.ml
	ocamlc -c -g Pretty.ml

	ocamlc -c -g Util.ml
	ocamlc -c -g StringModules.ml
	ocamlc -c -g GlobalData.mli
	ocamlc -c -g Klass.mli
	ocamlc -c -g KlassData.mli
	ocamlc -c -g BuiltIns.mli
	ocamlc -c -g BuiltIns.ml
	ocamlc -c -g Klass.ml
	ocamlc -c -g KlassData.ml
	ocamlc -c -g Variables.ml
	ocamlc -c -g Sast.ml
	ocamlc -c -g BuildSast.mli
	ocamlc -c -g BuildSast.ml
	ocamlc -c -g Unanonymous.ml
	ocamlc -c -g Cast.ml
	ocamlc -c -g GenCast.ml

	ocamlc -c -g Debug.ml

	ocamlc -c -g classinfo.ml
	ocamlc -c -g inspect.ml
	ocamlc -c -g prettify.ml
	ocamlc -c -g streams.ml
	ocamlc -c -g canonical.ml
	ocamlc -c -g freevars.ml

_tools:
	#Make the tools
	ocamlc -g -o tools/prettify UID.cmo scanner.cmo parser.cmo Inspector.cmo Pretty.cmo WhiteSpace.cmo prettify.cmo
	ocamlc -g -o tools/inspect UID.cmo scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo inspect.cmo
	ocamlc -g -o tools/streams UID.cmo scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo streams.cmo
	ocamlc -g -o tools/canonical UID.cmo scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo canonical.cmo
	ocamlc -g -o tools/freevars UID.cmo scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo Util.cmo StringModules.cmo BuiltIns.cmo Klass.cmo KlassData.cmo Debug.cmo Variables.cmo freevars.cmo
	ocamlc -g -o tools/classinfo UID.cmo scanner.cmo parser.cmo Inspector.cmo WhiteSpace.cmo Util.cmo StringModules.cmo BuiltIns.cmo Klass.cmo KlassData.cmo classinfo.cmo

docsources = Ast.mli BuildSast.ml BuildSast.mli BuiltIns.ml BuiltIns.mli Cast.ml Debug.ml GenCast.ml GlobalData.mli Inspector.ml Inspector.mli Klass.ml Klass.mli KlassData.ml KlassData.mli Pretty.ml Sast.ml StringModules.ml UID.ml Unanonymous.ml Util.ml Variables.ml WhiteSpace.ml parser.ml parser.mli scanner.ml
docgen = ./doc/.docgen

_doc:
	#Generate the documentation
	mkdir -p doc
	ocamldoc -hide-warnings -dump $(docgen) -keep-code $(docsources)
	ocamldoc -hide-warnings -load $(docgen) -d doc -t "The Ray Compiler" -html -colorize-code -all-params
	ocamldoc -hide-warnings -load $(docgen) -dot -o "./doc/ray-modules.dot"
	ocamldoc -hide-warnings -load $(docgen) -dot -dot-types -o "./doc/ray-types.dot"

bleach:
	rm *.cmi *.cmo parser.ml parser.mli scanner.ml
	rm -r ./doc

clean:
	rm *.cmi *.cmo parser.ml parser.mli scanner.ml

cleantools:
	rm tools/{prettify,inspect,streams,canonical,freevars,classinfo}

