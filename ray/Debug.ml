
let get_example_path dir example = String.concat Filename.dir_sep ["test"; "tests"; "Brace"; dir; example]

let get_example_scan dir example =
  let input = open_in (get_example_path dir example) in
  let tokens = Inspector.from_channel input in
  let _ = close_in input in
  tokens

let get_example_parse dir example =
  let tokens = get_example_scan dir example in
  Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "")
