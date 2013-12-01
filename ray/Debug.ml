open Ast

let get_example_path dir example = String.concat Filename.dir_sep ["test"; "tests"; "Brace"; dir; example]

let get_example_scan dir example =
  let input = open_in (get_example_path dir example) in
  let tokens = Inspector.from_channel input in
  let _ = close_in input in
  tokens

let get_example_parse dir example =
  let tokens = get_example_scan dir example in
  Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "")

let get_example_longest_body dir example =
  let klasses = get_example_parse dir example in
  let methods aklass = List.flatten (List.map snd (Klass.klass_to_functions aklass)) in
  let all_methods = List.flatten (List.map methods klasses) in
  let with_counts = List.map (function func -> (Util.get_statement_count func.body, func)) all_methods in
  let maximum = List.fold_left max 0 (List.map fst with_counts) in
  List.map snd (List.filter (function (c, _) -> c == maximum) with_counts)
