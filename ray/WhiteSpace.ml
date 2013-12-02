open Parser

(** Convert a whitespace file into a brace file. Yes, this module is the opposite of descriptively named. *)

(**
Gracefully tell the programmer that they done goofed
@param msg The descriptive error message to convey to the programmer
*)
let wsfail msg = raise(Failure(msg))

(**
    Only allow spacing that is at the start of a line
    @param program A program as a list of tokens
    @return I honestly don't especially get this organization
*)
let indenting_space program =
  let rec space_indenting rtokens = function
    | NEWLINE::SPACE(n)::rest -> space_indenting (SPACE(n)::NEWLINE::rtokens) rest
    | COLON::SPACE(n)::rest -> space_indenting (SPACE(n)::COLON::rtokens) rest
    | SPACE(n)::rest -> space_indenting rtokens rest
    | token::rest -> space_indenting (token::rtokens) rest
    | [] -> List.rev rtokens in
  match (space_indenting [] (NEWLINE::program)) with
    | NEWLINE::rest -> rest
    | _ -> wsfail "Indenting should have left a NEWLINE at the start of program; did not."

(**
    Between LBRACE and RBRACE we ignore spaces and newlines; colons are errors in this context.
    It's not necessary that this be done after the above, but it is recommended.
    @param program A program in the form of a list of tokens
    @return A slightly slimmer program
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

(**
    Remove empty indentation -- SPACE followed by COLON or NEWLINE
    @param program A program as a list of tokens
    @return A program without superfluous indentation
*)
let trim_lines program =
  let rec lines_trim tokens rtokens =
    match tokens with
      | [] -> List.rev rtokens
      | SPACE(_)::NEWLINE::rest -> lines_trim rest (NEWLINE::rtokens)
      | SPACE(_)::COLON::rest -> lines_trim rest (COLON::rtokens)
      | token::rest -> lines_trim rest (token::rtokens) in
  lines_trim program []

(** Remove consecutive newlines *)
let squeeze_lines program =
  let rec lines_squeeze tokens rtokens =
    match tokens with
      | [] -> List.rev rtokens
      | NEWLINE::NEWLINE::rest -> lines_squeeze (NEWLINE::rest) rtokens
      | COLON::NEWLINE::rest -> lines_squeeze (COLON::rest) rtokens (* scanner handled this though *)
      | token::rest -> lines_squeeze rest (token::rtokens) in
  lines_squeeze program []

(**
  Remove the initial space from a line but semantically note it
  @return an ordered pair of the number of spaces at the beginning
  of the line and the tokens in the line
*)
let spacing = function
  | SPACE(n)::rest -> (n, rest)
  | list           -> (0, list)

(** Remove spaces, newlines, and colons but semantically note their presence.
    @param program A full program
    @return a list of triples, one for each line. Each triple's first item is
    the number of spaces at the beginning of the line; the second item are the
    tokens in the line; the third is whether the line ended in a colon.
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

(**
    Merge line continuatons from the above
    @param program_lines The individual lines of the program
    @return The lines of the program with whitespace collapsed
*)
let merge_lines program_lines =
  let rec lines_merge rlines = function
    | ((n1, _, _) as line1)::((n2, _, _) as line2)::rest when n1 >= n2 -> lines_merge (line1::rlines) (line2::rest)
    | (n, line1, false)::(_, line2, colon)::rest -> lines_merge rlines ((n, line1@line2, colon)::rest)
    | ((_, _, true) as line)::rest -> lines_merge (line::rlines) rest
    | line::[] -> lines_merge (line::rlines) []
    | [] -> List.rev rlines in
  lines_merge [] program_lines

(**
    Check if a given line needs a semicolon at the end
*)
let rec needs_semi = function
  | [] -> true              (* Null case*)
  | RBRACE::[] -> false     (* The end of bodies do not require semicolons *)
  | SEMI::[] -> false       (* A properly terminated line does not require an additional semicolon *) 
  | _::rest -> needs_semi rest (* Go through *)

(**
    Build a block. Consecutive lines of the same indentation with only the last ending
    in a colon are a `block'. Blocks are just `lines' merged together but joined with
    a semi colon when necessary.
    @param lines The full set of lines
    @return A list of blocks
 *)
let block_merge lines =
  let braced = function
    | (n, toks, true) -> (n, toks, true, false)
    | (n, toks, false) -> (n, toks, false, needs_semi toks) in
  let lines = List.map braced lines in
  let rec merge_blocks rblocks = function
    | (n1, line1, false, s1)::(n2, line2, colon, s2)::rest when n1 = n2 ->
      let newline = line1 @ (if s1 then [SEMI] else []) @ line2 in
      merge_blocks rblocks ((n1, newline, colon, s2)::rest)
    | (n, line, colon, _)::rest -> merge_blocks ((n, line, colon)::rblocks) rest
    | [] -> List.rev rblocks in
  merge_blocks [] lines

(** Make sure every line is terminated with a semi-colon *)
let terminate_blocks blocks =
  let rec block_terminate rblocks = function
    | (n, toks, false)::rest ->
      let terminated = if (needs_semi toks) then toks@[SEMI] else toks in
      block_terminate ((n, terminated, false)::rblocks) rest
    | other::rest ->
      block_terminate (other::rblocks) rest
    | [] -> List.rev rblocks in
  block_terminate [] blocks

(** Pops the stack and adds rbraces when necessary *)
let rec arrange n stack rtokens =
  match stack with
    | top::rest when n <= top -> arrange n rest (RBRACE::rtokens)
    | _ -> (stack, rtokens)

(** Take results of pipeline and finally adds braces. If blocks are merged
    then either consecutive lines differ in scope or there are colons.
    so now everything should be easy peasy (lemon squeezy).
*)
let space_to_brace = function
  | [] -> []
  | linelist -> let rec despace_enbrace stack rtokens = function
    | [] -> List.rev ((List.map (function _ -> RBRACE) stack) @ rtokens)
    | (n, line, colon)::rest ->
      let (stack, rtokens) = arrange n stack rtokens in
      let (lbrace, stack) = if colon then ([LBRACE], n::stack) else ([], stack) in
      despace_enbrace stack (lbrace@(List.rev line)@rtokens) rest
    in despace_enbrace [] [] linelist

(** Drop the EOF from a stream of tokens, failing if not possible *)
let drop_eof program =
  let rec eof_drop rtokens = function
    | EOF::[] -> List.rev rtokens
    | EOF::rest -> raise(Failure("Misplaced EOF"))
    | [] -> raise(Failure("No EOF available."))
    | tk::tks -> eof_drop (tk::rtokens) tks in
  eof_drop [] program

(** Append an eof token to a program *)
let append_eof program =
  let rec eof_add rtokens = function
    | [] -> List.rev (EOF::rtokens)
    | tk::tks -> eof_add (tk::rtokens) tks in
  eof_add [] program

(** Run the entire pipeline *)
let convert program =
  (* Get rid of the end of file *)
  let noeof = drop_eof program in
  (* Indent in response to blocks *)
  let indented = indenting_space noeof in
  (* Collapse whitespace around braces *)
  let despaced = despace_brace indented in
  (* Get rid of trailing whitespace *)
  let trimmed = trim_lines despaced in
  (* Remove consequetive newlines *)
  let squeezed = squeeze_lines trimmed in
  (* Turn tokens into semantics *)
  let lines = tokens_to_lines squeezed in
  (* Consolidate those semantics *)
  let merged = merge_lines lines in
  (* Turn the semantics into blocks *)
  let blocks = block_merge merged in
  (* Put in the semicolons *)
  let terminated = terminate_blocks blocks in
  (* Turn the blocks into braces *)
  let converted = space_to_brace terminated in
  (* Put the eof on *)
  append_eof converted

(** A function to act like a lexfun *)
let lextoks toks =
  let tokens = ref (convert toks) in
  function _ ->
    match !tokens with
      | [] -> raise(Failure("Not even EOF given."))
      | tk::tks -> tokens := tks; tk
