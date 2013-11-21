let _ = 
  let tokens = Inspector.from_channel stdin in
  let classes = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
  let pp_classes = List.map Pretty.pp_class_def classes in
  print_string (String.concat "\n\n" pp_classes); print_newline ()
