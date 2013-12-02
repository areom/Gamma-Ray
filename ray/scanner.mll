{
  open Parser

  (** The general lexographic scanner for Gamma *)

  (**
  	Build a string from a list of characters...why aren't we using fold for this?
	from: http://caml.inria.fr/mantis/view.php?id=5367
	@param l The list to be glued
	@return A string of the characters in the list glued together
  *)
  let implode l =
    let res = String.create (List.length l) in
    let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
    imp 0 l

  (**
    Explode a string into a list of characters
  	@param s The string to be exploded
	@return A list of the characters in the string in order
  *)
  let explode s =
    let rec exploder idx l =
      if idx < 0
        then l
        else exploder (idx-1) (s.[idx] :: l) in
    exploder (String.length s - 1) []

  (**
  	A generic function to count the character-spaces of a character. (I.e. weight tabs more heavily)
  *)
  let spacecounter = function
    | '\t' -> 8
    | _    -> 1

  (**
    Count the space width of a string using the spacecounter function
	@param s The string to be evaluated
	@return The effective width of the string when rendered
  *)
  let spacecount s =
    let spaces = List.map spacecounter (explode s) in
    List.fold_left (+) 0 spaces

  (**/**)
  let line_number = ref 1
  (**/**)

  (**
    Count the lines in a series of vertical spacing characters.
    Please note that as of now, it is not intelligent enough to understand
    that \n\r should be counted as one. It seems like an oversized-amount
    of work for something we will never effectively need.
    @param v The vertical spacing series string
  *)
  let count_lines v = (line_number := !line_number + String.length v)

  (**
    Gracefully tell the programmer that they done goofed
	@param msg The descriptive error message to convey to the programmer
  *)
  let lexfail msg =
    raise (Failure("Line " ^ string_of_int !line_number ^ ": " ^ msg))
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let ualphanum = '_' | alpha | digit

(* horizontal spacing: space & tab *)
let hspace = [' ' '\t']

(* vertical spaces: newline (line feed), carriage return, vertical tab, form feed *)
let vspace = ['\n' '\r' '\011' '\012']


rule token = parse
  (* Handling whitespace mode *)
  | hspace+ as s                 { SPACE(spacecount s) }
  | ':' hspace* (vspace+ as v)   { count_lines v; COLON }
  | vspace+ as v                 { count_lines v; NEWLINE }

  (* Comments *)
  | "/*"                       { comment 0 lexbuf }

  (* Boolean Tests & Values *)
  | "refinable"                { REFINABLE }
  | "and"                      { AND }
  | "or"                       { OR }
  | "xor"                      { XOR }
  | "nand"                     { NAND }
  | "nor"                      { NOR }
  | "not"                      { NOT }
  | "true"                     { BLIT(true) }
  | "false"                    { BLIT(false) }
  | "="                        { EQ }
  | "<>"                       { NEQ }
  | "=/="                      { NEQ }
  | '<'                        { LT }
  | "<="                       { LEQ }
  | ">"                        { GT }
  | ">="                       { GEQ }

  (* Grouping [args, arrays, code, etc] *)
  | "[]"                       { ARRAY }
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

  (* Arithmetic assignment *)
  | "+="                       { PLUSA }
  | "-="                       { MINUSA }
  | "*="                       { TIMESA }
  | "/="                       { DIVIDEA }
  | "%="                       { MODA }
  | "^="                       { POWERA }

  (* Control flow *)
  | "if"                       { IF }
  | "else"                     { ELSE }
  | "elsif"                    { ELSIF }
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
  | "this"                     { THIS }

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
  | '_'? lower ualphanum* as vid    { ID(vid) }
  | upper ualphanum* as tid         { TYPE(tid) }

  (* Literals *)
  | digit+ as inum             { ILIT(int_of_string inum) }
  | digit+ '.' digit+ as fnum  { FLIT(float_of_string fnum) }
  | '"'                        { stringlit [] lexbuf }

  (* Some type of end, for sure *)
  | eof                        { EOF }
  | _ as char { lexfail("Illegal character " ^ Char.escaped char) }

and comment level = parse
  (* Comments can be nested *)
  | "/*"          { comment (level+1) lexbuf }
  | "*/"          { if level = 0 then token lexbuf else comment (level-1) lexbuf }
  | eof           { lexfail("File ended inside comment.") }
  | vspace+ as v  { count_lines v; comment(0) lexbuf } 
  | _             { comment (0) lexbuf }

and stringlit chars = parse
  (* Accept valid C string literals as that is what we will output directly *)
  | '\\'           { escapechar chars lexbuf }
  | eof            { lexfail("File ended inside string literal") }
  | vspace as char { lexfail("Line ended inside string literal (" ^ Char.escaped char ^ " used): " ^ implode(List.rev chars)) }
  | '"'            { SLIT(implode(List.rev chars)) }
  | _ as char      { stringlit (char::chars) lexbuf }

and escapechar chars = parse
  (* Accept valid C escape sequences *)
  | ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\' '"' '0'] as char {
      stringlit (char :: '\\' :: chars) lexbuf
    }
  | eof       { lexfail("File ended while seeking escape character") }
  | _ as char { lexfail("Illegal escape character:  \\" ^ Char.escaped(char)) }

