let debug_print tokens =
  Inspector.print_tokens "Input:      " tokens;
  let tokens = WhiteSpace.indenting_space tokens in
  Inspector.print_tokens "Indented:   " tokens;
  let tokens = WhiteSpace.despace_brace tokens in
  Inspector.print_tokens "In-Brace:   " tokens;
  let tokens = WhiteSpace.trim_lines tokens in
  Inspector.print_tokens "Trimmed:    " tokens;
  let tokens = WhiteSpace.squeeze_lines tokens in
  Inspector.print_tokens "Squeezed:   " tokens;
  let lines = WhiteSpace.tokens_to_lines tokens in
  Inspector.print_lines  "Lines:      " lines;
  let lines = WhiteSpace.merge_lines lines in
  Inspector.print_lines  "Merged:     " lines;
  let lines = WhiteSpace.block_merge lines in
  Inspector.print_lines  "Blocks:     " lines;
  let tokens = WhiteSpace.space_to_brace lines in
  Inspector.print_tokens "Converted:  " tokens

let _ =
  let tokens = Inspector.from_channel stdin in
  match Array.length Sys.argv with
    | 1 -> Inspector.print_tokens "" (WhiteSpace.convert tokens)
    | _ -> debug_print tokens
