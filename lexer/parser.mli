type token =
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | COMMA
  | LBRACE
  | RBRACE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | POWER
  | EQ
  | NEQ
  | GT
  | LT
  | GEQ
  | LEQ
  | AND
  | OR
  | NAND
  | NOR
  | XOR
  | NOT
  | TRUE
  | FALSE
  | IF
  | ELSE
  | ELSIF
  | WHILE
  | ASSIGN
  | RETURN
  | CLASS
  | EXTEND
  | SUPER
  | INIT
  | PRIVATE
  | PROTECTED
  | PUBLIC
  | NULL
  | VOID
  | THIS
  | NEW
  | MAIN
  | ARRAY
  | REFINABLE
  | REFINE
  | REFINES
  | TO
  | SEMI
  | DOT
  | EOF
  | TYPE of (string)
  | ILIT of (int)
  | FLIT of (float)
  | BLIT of (bool)
  | SLIT of (string)
  | ID of (string)

val cdecl :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.class_def
