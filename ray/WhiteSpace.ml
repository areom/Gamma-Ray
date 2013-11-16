open Parser

let wsfail msg = raise(Failure(msg))

(* Only allow spacing that is at the start of a line *)
let indenting_space program =
  let rec space_indenting rtokens = function
    | NEWLINE::SPACE(n)::rest -> space_indenting (SPACE(n)::NEWLINE::rtokens) rest
    | SPACE(n)::rest -> space_indenting rtokens rest
    | token::rest -> space_indenting (token::rtokens) rest
    | [] -> List.rev rtokens in
  match (space_indenting [] (NEWLINE::program)) with
    | NEWLINE::rest -> rest
    | _ -> wsfail "Indenting should have left a NEWLINE at the start of program; did not."

(* Between LBRACE and RBRACE we ignore spaces and newlines; colons are errors in this context.
 * It's not necessary that this be done after the above, but it is recommended.
 *)
let despace_brace program =
  let rec brace_despace depth tokens rtokens last =
    if depth > 0 then
      match tokens with
        | SPACE(_)::rest -> brace_despace depth rest rtokens last
        | NEWLINE::rest -> brace_despace depth rest rtokens last
        | COLON::_ -> wsfail "Colon inside brace scoping."
        | LBRACE::rest -> brace_despace (depth+1) rest (LBRACE::rtokens) last
        | RBRACE::rest -> let rtokens = if depth = 1
          then SPACE(last)::NEWLINE::RBRACE::rtokens
          else RBRACE::rtokens in
          brace_despace (depth-1) rest rtokens last
        | token::rest -> brace_despace depth rest (token::rtokens) last
        | [] -> List.rev rtokens
    else
      match tokens with
        | SPACE(n)::rest -> brace_despace depth rest (SPACE(n)::rtokens) n
        | LBRACE::rest -> brace_despace (depth+1) rest (LBRACE::rtokens) last
        | token::rest -> brace_despace depth rest (token::rtokens) last
        | [] -> List.rev rtokens in
  brace_despace 0 program [] 0

(* Remove empty lines -- change SPACE NEWLINE to NEWLINE *)
let trim_lines program =
  let rec lines_trim tokens rtokens =
    match tokens with
      | [] -> List.rev rtokens
      | SPACE(_)::NEWLINE::rest -> lines_trim rest (NEWLINE::rtokens)
      | token::rest -> lines_trim rest (token::rtokens) in
  lines_trim program []

(* Remove consecutive newlines *)
let squeeze_lines program =
  let rec lines_squeeze tokens rtokens =
    match tokens with
      | [] -> List.rev rtokens
      | NEWLINE::NEWLINE::rest -> lines_squeeze (NEWLINE::rest) rtokens
      | COLON::NEWLINE::rest -> lines_squeeze (COLON::rest) rtokens (* scanner handled this though *)
      | token::rest -> lines_squeeze rest (token::rtokens) in
  lines_squeeze program []

(* Remove the initial space from a line but semantically note it *)
let spacing = function
  | SPACE(n)::rest -> (n, rest)
  | list           -> (0, list)

(* Remove spaces, newlines, and colons but semantically note their presence.
 * Returns a list of triples, one for each line. Each triple's first item is
 * the number of spaces at the beginning of the line; the second item are the
 * tokens in the line; the third is whether the line ended in a colon.
 *)
let tokens_to_lines program =
  let rec lines_from_tokens rline rlines = function
    | NEWLINE::rest ->
      (match rline with
        | [] -> lines_from_tokens [] rlines rest
        | _  -> let (spacer, line) = spacing (List.rev rline) in
                lines_from_tokens [] ((spacer, line, false)::rlines) rest)
    | COLON::rest ->
      (match rline with
        | [] -> lines_from_tokens [] rlines rest
        | _  -> let (spacer, line) = spacing (List.rev rline) in
                lines_from_tokens [] ((spacer, line, true)::rlines) rest)
    | [] ->
      (match rline with
        | [] -> List.rev rlines
        | _  -> let (spacer, line) = spacing (List.rev rline) in
                lines_from_tokens [] ((spacer, line, false)::rlines) [])
    | token::rest -> lines_from_tokens (token::rline) rlines rest in
  lines_from_tokens [] [] program

(* Merge line continuatons from the above *)
let merge_lines program_lines =
  let rec lines_merge rlines = function
    | ((n1, _, _) as line1)::((n2, _, _) as line2)::rest when n1 >= n2 -> lines_merge (line1::rlines) (line2::rest)
    | (n, line1, false)::(_, line2, colon)::rest -> lines_merge rlines ((n, line1@line2, colon)::rest)
    | ((_, _, true) as line)::rest -> lines_merge (line::rlines) rest
    | line::[] -> lines_merge (line::rlines) []
    | [] -> List.rev rlines in
  lines_merge [] program_lines

(* Check if a given line ends with a brace *)
let rec ends_brace = function
  | [] -> false
  | RBRACE::[] -> true
  | _::rest -> ends_brace rest

(* Consecutive lines of the same indentation with only the last ending in a colon are a `block'
 * Blocks are just `lines' merged together but joined with a semi colon when necessary
 *)
let block_merge lines =
  let braced = function
    | (n, toks, true) -> (n, toks, true, false)
    | (n, toks, false) -> (n, toks, false, not (ends_brace toks)) in
  let lines = List.map braced lines in
  let rec merge_blocks rblocks = function
    | (n1, line1, false, s1)::(n2, line2, colon, s2)::rest when n1 = n2 ->
      let newline = line1 @ (if s1 then [SEMI] else []) @ line2 in
      merge_blocks rblocks ((n1, newline, colon, s2)::rest)
    | (n, line, colon, _)::rest -> merge_blocks ((n, line, colon)::rblocks) rest
    | [] -> List.rev rblocks in
  merge_blocks [] lines

(* Pops the stack and adds rbraces when necessary *)
let rec arrange n stack rtokens =
  match stack with
    | top::rest when top > n -> arrange n rest (RBRACE::rtokens)
    | _ -> (stack, rtokens)

(* Take results of pipeline and finally adds braces. If blocks are merged
 * then either consecutive lines differ in scope or there are colons.
 * so now everything should be easy peasy (lemon squeezy).
 *)
let space_to_brace = function
  | [] -> []
  | linelist -> let rec despace_enbrace stack rtokens = function
    | (n, line, true)::[] -> despace_enbrace (n::stack) ((List.rev line)@rtokens) []
    | (n, line, false)::[] -> despace_enbrace stack ((List.rev line)@rtokens) []
    | [] -> List.rev ((List.map (function _ -> RBRACE) stack) @ rtokens)
    | (n, line, colon)::rest ->
      let (stack, rtokens) = arrange n stack rtokens in
      let (rbrace, stack) = if colon then ([RBRACE], n::stack) else ([], stack) in
      despace_enbrace stack (rbrace@(List.rev line)@rtokens) rest
    in despace_enbrace [] [] linelist

(* Run the entire pipeline *)
let convert program =
  let indented = indenting_space program in
  let despaced = despace_brace indented in
  let trimmed = trim_lines despaced in
  let squeezed = squeeze_lines trimmed in
  let lines = tokens_to_lines squeezed in
  let merged = merge_lines lines in
  let blocks = block_merge merged in
  space_to_brace blocks
