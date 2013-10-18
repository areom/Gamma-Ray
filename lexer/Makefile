ocamllex scanner.mll # create scanner.ml
ocamlyacc parser.mly # create parser.ml and parser.mli
ocamlc -c -g ast.mli # compile AST types
ocamlc -c -g parser.mli # compile parser types
ocamlc -c -g scanner.ml # compile the scanner
ocamlc -c -g parser.ml # compile the parser
ocamlc -c -g calc.ml # compile the interpreter
ocamlc -o calc parser.cmo scanner.cmo calc.cmo
