let debug_print tokens =
  Tokens.print_tokens "Input:      " tokens;
  let tokens = WhiteSpace.indenting_space tokens in
  Tokens.print_tokens "Indented:   " tokens;
  let tokens = WhiteSpace.despace_brace tokens in
  Tokens.print_tokens "In-Brace:   " tokens;
  let tokens = WhiteSpace.trim_lines tokens in
  Tokens.print_tokens "Trimmed:    " tokens;
  let tokens = WhiteSpace.squeeze_lines tokens in
  Tokens.print_tokens "Squeezed:   " tokens;
  let lines = WhiteSpace.tokens_to_lines tokens in
  Tokens.print_lines  "Lines:      " lines;
  let lines = WhiteSpace.merge_lines lines in
  Tokens.print_lines  "Merged:     " lines;
  let lines = WhiteSpace.block_merge lines in
  Tokens.print_lines  "Blocks:     " lines;
  let tokens = WhiteSpace.space_to_brace lines in
  Tokens.print_tokens "Converted:  " tokens

let _ =
  let tokens = Tokens.from_channel stdin in
  match Array.length Sys.argv with
    | 1 -> Tokens.print_tokens "" (WhiteSpace.convert tokens)
    | _ -> debug_print tokens
