{ open Parser } (* Get the token types *)

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let alphanum = alpha | digit

rule token = parse

  (* Comments & White Space *)
  | [' ' '\t' '\r' '\n']       { token lexbuf }
  | "/*"                       { comment 0 lexbuf }

  (* Boolean Tests & Values *)
  | "refinable"                { REFINABLE }
  | "and"                      { AND }
  | "or"                       { OR }
  | "xor"                      { XOR }
  | "nand"                     { NAND }
  | "nor"                      { NOR }
  | '!'                        { NOT }
  | "true"                     { TRUE }
  | "false"                    { FALSE }
  | "=="                       { EQ }
  | "!="                       { NEQ }
  | '<'                        { LT }
  | "<="                       { LEQ }
  | ">"                        { GT }
  | ">="                       { GEQ }

  (* Grouping [args, arrays, code, etc] *)
  | '['                        { LBRACKET }
  | ']'                        { RBRACKET }
  | '('                        { LPAREN }
  | ')'                        { RPAREN }
  | '{'                        { LBRACE }
  | '}'                        { RBRACE }

  (* Punctuation for the sytnax *)
  | ';'                        { SEMI }
  | ','                        { COMMA }

  (* Arithmetic operations *)
  | '+'                        { PLUS }
  | '-'                        { MINUS }
  | '*'                        { TIMES }
  | '/'                        { DIVIDE }
  | '='                        { ASSIGN }
  | '%'                        { MOD }

  (* Control flow *)
  | "if"                       { IF }
  | "else"                     { ELSE }
  | "while"                    { WHILE }
  | "return"                   { RETURN }

  (* OOP Stuff *)
  | "class"                    { CLASS }
  | "extends"                  { EXTEND }
  | "super"                    { SUPER }
  | "init"                     { INIT }

  (* Pre defined types / values *)
  | "null"                     { NULL }
  | "void"                     { VOID }

  (* Refinement / specialization related *)
  | "refine"                   { REFINE }
  | "refinements"              { REFINES }
  | "to"                       { TO }

  (* Access *)
  | "private"                  { PRIVATE }
  | "public"                   { PUBLIC }
  | "protected"                { PROTECTED }

  (* Miscellaneous *)
  | '.'                        { DOT }
  | "main"                     { MAIN }
  | "new"                      { NEW }

  (* Variable and Type IDs *)
  | lower alphanum+ as vid     { VAR(vid) }
  | upper alphanum+ as tid     { TYPE(tid) }

  (* Literals *)
  | digit+ as inum             { ILIT(int_of_string inum) }
  | digit+ '.' digit+ as fnum  { FLIT(float_of_string fnum) }
  | '"'                        { stringlit [] lexbuf }

  (* Some type of end, for sure *)
  | eof                        { EOF }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment level = parse
  (* Comments can be nested *)
  | "/*"   { comment (level+1) lexbuf }
  | "*/"   { if level = 0 then token lexbuf else comment (level-1) lexbuf }
  | _      { comment lexbuf }

and stringlit chars = parse
  (* We only accept valid C string literals, as that is what we will output directly *)
  | '\\'       { escapechar chars lexbuf }
  | '\n'       { raise (Failure( "End of string literal " ^ implode(List.rev(chars)) )) }
  | '"'        { SLIT(List.rev chars) }
  | _ as char  { stringlit (char::chars) lexbuf }

and escapechar chars = parse
  (* Only accept valid C escape sequences *)
  | ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\' '\'' '"' '0'] as char {
      stringlit (char :: '\\' :: chars) lexbuf
    }
  | _ as char { raise (Failure("illegal escape character:  \\" ^ Char.escaped char)) }
