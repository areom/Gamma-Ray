open Parser

let stoken tk = match tk with
  | SPACE(n) -> "SPACE(" ^ string_of_int n ^ ")"
  | COLON -> "COLON"
  | NEWLINE -> "NEWLINE"
  | THIS -> "THIS"
  | ARRAY -> "ARRAY"
  | REFINABLE -> "REFINABLE"
  | AND -> "AND"
  | OR -> "OR"
  | XOR -> "XOR"
  | NAND -> "NAND"
  | NOR -> "NOR"
  | NOT -> "NOT"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | LT -> "LT"
  | LEQ -> "LEQ"
  | GT -> "GT"
  | GEQ -> "GEQ"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | SEMI -> "SEMI"
  | COMMA -> "COMMA"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVIDE -> "DIVIDE"
  | MOD -> "MOD"
  | POWER -> "POWER"
  | PLUSA -> "PLUSA"
  | MINUSA -> "MINUSA"
  | TIMESA -> "TIMESA"
  | DIVIDEA -> "DIVIDEA"
  | MODA -> "MODA"
  | POWERA -> "POWERA"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | ELSIF -> "ELSIF"
  | WHILE -> "WHILE"
  | RETURN -> "RETURN"
  | CLASS -> "CLASS"
  | EXTEND -> "EXTEND"
  | SUPER -> "SUPER"
  | INIT -> "INIT"
  | NULL -> "NULL"
  | VOID -> "VOID"
  | REFINE -> "REFINE"
  | REFINES -> "REFINES"
  | TO -> "TO"
  | PRIVATE -> "PRIVATE"
  | PUBLIC -> "PUBLIC"
  | PROTECTED -> "PROTECTED"
  | DOT -> "DOT"
  | MAIN -> "MAIN"
  | NEW -> "NEW"
  | ASSIGN -> "ASSIGN"
  | ID(vid) -> Printf.sprintf "ID(%s)" vid
  | TYPE(tid) -> Printf.sprintf "TYPE(%s)" tid
  | BLIT(bool) -> Printf.sprintf "BLIT(%B)" bool
  | ILIT(inum) -> Printf.sprintf "ILIT(%d)" inum
  | FLIT(fnum) -> Printf.sprintf "FLIT(%f)" fnum
  | SLIT(str) -> Printf.sprintf "SLIT(%s)" (String.escaped str)
  | EOF -> "EOF"


let token_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
  let rec list_tokens rtokens =
    match (lexfun lexbuf) with
      | EOF -> List.rev (EOF::rtokens)
      | tk -> list_tokens (tk::rtokens) in
  list_tokens []

let rec token_printer = function
  | [] -> ()
  | tk::rest -> print_string (stoken tk); print_string " "; token_printer rest

let line_printer = function
  | (space, toks, colon) ->
    print_string ("(" ^ string_of_int space ^ "," ^ string_of_bool colon ^ ") ");
    token_printer toks

let from_channel source = token_list Scanner.token (Lexing.from_channel source)

let print_tokens header toks = print_string header ; token_printer toks ; print_newline ()

let print_lines header lines =
  let spaces = String.make (String.length header) ' ' in
  let rec lines_printer prefix = function
    | line::rest ->
      print_string prefix;
      line_printer line;
      print_newline ();
      lines_printer spaces rest
    | [] -> () in
  lines_printer header lines
