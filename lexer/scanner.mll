{ open Parser } (* Get the token types *)

rule token = parse
  | [' ' '\t' '\r' '\n']       { token lexbuf }
  | "/*"                       { comment 0 lexbuf }
  | "refinable"                { RFNABLE }
  | "and"	               { AND }
  | "or" 	               { OR }
  | "xor"	               { XOR }
  | "nand"                     { NAND }
  | "nor"	               { NOR }
  | '!'	                       { NOT }
  | "true"	               { TRUE }
  | "false"                    { FALSE }
  | '['                        { LBRACKET }
  | ']'                        { RBRACKET }
  | '('                        { LPAREN }
  | ')'                        { RPAREN }	 
  | "to"                       { TO }
  | "NULL"                     { NULL }
  | "void"                     { VOID }
  | "init"                     { INIT }
  | "main"                     { MAIN }
  | '.'                        { DOT }
  | "super"                    { SUPER }
  | "switch"                   { SW }
  | "case"                     { CASE }
  | "default"                  { DEF }
  | "break"                    { BRK }
  | "const"                    { CONST }
  | "continue"                 { CONT }
  | "class"                    { CLASS }
  | "extends"                  { EXTEND }
  | "refinements"              { D_RFN }
  | "private"                  { PRIVATE }
  | "public"                   { PUBLIC }
  | "protected"	               { PROTECTED }
  | "new"                      { NEW }
  |  '('                       { LPAREN }
  | ')'                        { RPAREN }
  | '{'                        { LBRACE }
  | '}'                        { RBRACE }
  | ';'                        { SEMI }
  | ','                        { COMMA }
  | '+'                        { PLUS }
  | '-'                        { MINUS }
  | '*'                        { TIMES }
  | '/'                        { DIVIDE }
  | '='                        { ASSIGN }
  | "=="                       { EQ }
  | "!="                       { NEQ }
  | '<'                        { LT }
  | "<=                        { LEQ }
  | ">"                        { GT }
  | ">="                       { GEQ }
  | "if"                       { IF }
  | "else"                     { ELSE }
  | "for"                      { FOR }
  | "while"                    { WHILE }
  | "return"                   { RETURN }
  | '?'                        { QN }
  | ':'                        { COL }
  | "&&"                       { RELAND }
  | "||"                       { RELOR }
  | '^'                        { CRT }
  | '%'                        { MOD }
  | eof                        { EOF }
  | ['0'-'9']+ as lxm          { LITERAL(int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment level = parse
  | "/*"   { comment (level+1) lexbuf }
  | "*/"   { if level = 0 then token lexbuf else comment (level-1) lexbuf }
  | _      { comment lexbuf }
