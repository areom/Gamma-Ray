{
  open Parser

  (* from: http://caml.inria.fr/mantis/view.php?id=5367 *)
  let implode l =
    let res = String.create (List.length l) in
    let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
    imp 0 l

  let lexfail msg =
    raise (Failure(msg))
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let ualphanum = '_' | alpha | digit

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
  | "="                        { EQ }
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
  | '%'                        { MOD }
  | '^'                        { POWER }

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
  | "refinement"               { REFINES }
  | "to"                       { TO }

  (* Access *)
  | "private"                  { PRIVATE }
  | "public"                   { PUBLIC }
  | "protected"                { PROTECTED }

  (* Miscellaneous *)
  | '.'                        { DOT }
  | "main"                     { MAIN }
  | "new"                      { NEW }
  | ":="                       { ASSIGN }

  (* Variable and Type IDs *)
  | lower ualphanum* as vid    { ID(vid) }
  | upper ualphanum* as tid    { TYPE(tid) }

  (* Literals *)
  | digit+ as inum             { ILIT(int_of_string inum) }
  | digit+ '.' digit+ as fnum  { FLIT(float_of_string fnum) }
  | '"'                        { stringlit [] lexbuf }

  (* Some type of end, for sure *)
  | eof                        { EOF }
  | _ as char { lexfail("illegal character " ^ Char.escaped char) }

and comment level = parse
  (* Comments can be nested *)
  | "/*"   { comment (level+1) lexbuf }
  | "*/"   { if level = 0 then token lexbuf else comment (level-1) lexbuf }
  | _      { comment (0) lexbuf }
and stringlit chars = parse
  (* Accept valid C string literals as that is what we will output directly *)
  | '\\'       { escapechar chars lexbuf }
  | '\n'       { lexfail("End of string literal " ^ implode(List.rev chars)) }
  | '"'        { SLIT(implode(List.rev chars)) }
  | _ as char  { stringlit (char::chars) lexbuf }

and escapechar chars = parse
  (* Accept valid C escape sequences *)
  | ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\' '\'' '"' '0'] as char {
      stringlit (char :: '\\' :: chars) lexbuf
    }
  | _ as char { lexfail("illegal escape character:  \\" ^ Char.escaped(char)) }
