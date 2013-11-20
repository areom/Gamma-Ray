let debug_print tokens =
  let ptoken header tokens =
    Inspector.pprint_token_list header tokens;
    print_newline () in
  let plines header lines =
    Inspector.pprint_token_lines header lines;
    print_newline () in
  begin
    ptoken "Input:      " tokens;
    let tokens = WhiteSpace.drop_eof tokens in
    ptoken "No EOF      " tokens;
    let tokens = WhiteSpace.indenting_space tokens in
    ptoken "Indented:   " tokens;
    let tokens = WhiteSpace.despace_brace tokens in
    ptoken "In-Brace:   " tokens;
    let tokens = WhiteSpace.trim_lines tokens in
    ptoken "Trimmed:    " tokens;
    let tokens = WhiteSpace.squeeze_lines tokens in
    ptoken "Squeezed:   " tokens;
    let lines = WhiteSpace.tokens_to_lines tokens in
    plines "Lines:      " lines;
    let lines = WhiteSpace.merge_lines lines in
    plines "Merged:     " lines;
    let lines = WhiteSpace.block_merge lines in
    plines "Blocks:     " lines;
    let tokens = WhiteSpace.space_to_brace lines in
    ptoken "Converted:  " tokens;
    let tokens = WhiteSpace.append_eof tokens in
    ptoken "With EOF:   " tokens
  end

let _ =
  let tokens = Inspector.from_channel stdin in
  match Array.length Sys.argv with
    | 1 -> Inspector.pprint_token_list "" (WhiteSpace.convert tokens)
    | _ -> debug_print tokens
