let _ =
  let tokens = Inspector.from_channel stdin in
  let classes = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
  let inspect_classes = List.map Inspector.inspect_class_def classes in
  print_string (String.concat "\n\n" inspect_classes); print_newline ()

