open Parser

let stoken tk = match tk with
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

let rec stream_printer (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
  match (lexfun lexbuf) with
    | EOF -> print_string "EOF" ; print_newline ()
    | _ as tk -> print_string (stoken tk); print_string " "; stream_printer lexfun lexbuf

let print_tokens source = stream_printer Scanner.token (Lexing.from_channel source)

let _ = print_tokens stdin
