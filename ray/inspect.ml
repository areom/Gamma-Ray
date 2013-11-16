let _ =
  let tokens = ref (WhiteSpace.convert (Inspector.from_channel stdin)) in
  let token _ = 
    match !tokens with
      | [] -> raise(Failure("Not even EOF given."))
      | tk::tks -> tokens := tks; tk in
  let classes = Parser.cdecls token (Lexing.from_string "") in
  let inspect_classes = List.map Inspector.inspect_class_def classes in
  print_string (String.concat "\n\n" inspect_classes); print_newline ()

