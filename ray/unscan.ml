open Parser

let descan = Inspector.descan

let rec indenter depth indent =
  for i = 1 to depth do print_string indent done

(* Unscan a sequence of tokens. Requires sanitized stream *)
let rec clean_unscan depth indent = function
  (* ARRAY / LBRACKET RBRACKET ambiguity... *)
  | LBRACKET::RBRACKET::rest ->
    print_string ((descan LBRACKET) ^ " " ^ (descan RBRACKET));
    clean_unscan depth indent rest
  | LBRACE::rest ->
    print_string (descan LBRACE);
    print_newline ();
    indenter (depth+1) indent;
    clean_unscan (depth+1) indent rest
  | SEMI::RBRACE::rest ->
    print_string (descan SEMI);
    clean_unscan depth indent (RBRACE::rest)
  | RBRACE::RBRACE::rest ->
    print_newline ();
    indenter (max (depth-1) 0) indent;
    print_string (descan RBRACE);
    clean_unscan (max (depth-1) 0) indent (RBRACE::rest)
  | RBRACE::rest ->
    print_newline ();
    indenter (depth-1) indent;
    print_string (descan RBRACE);
    print_newline ();
    indenter (depth-1) indent;
    clean_unscan (max (depth-1) 0) indent rest
  | SEMI::rest ->
    print_string (descan SEMI);
    print_newline ();
    indenter depth indent;
    clean_unscan depth indent rest
  | EOF::[] ->
    print_newline ()
  | EOF::_ ->
    raise(Failure("Premature end of file."))
  | token::rest ->
    print_string (descan token);
    print_string " ";
    clean_unscan depth indent rest
  | [] ->
    print_newline ()

let _ =
  let tokens = Inspector.from_channel stdin in
  clean_unscan 0 "  " (WhiteSpace.convert tokens)
