open Parser
open Ast

(** Provides functionality for examining values used in the compilation pipeline. *)

(* TOKEN stuff *)
(** Convert a given token to a string representation for output *)
let token_to_string = function
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
    | SLIT(str) -> Printf.sprintf "SLIT(\"%s\")" (str)
    | EOF -> "EOF"

(** Convert token to its (assumed) lexographical source *)
let descan = function
    | COLON -> ":"
    | NEWLINE -> "\n"
    | SPACE(n) -> String.make n ' '
    | REFINABLE -> "refinable"
    | AND -> "and"
    | OR -> "or"
    | XOR -> "xor"
    | NAND -> "nand"
    | NOR -> "nor"
    | NOT -> "not"
    | EQ -> "="
    | NEQ -> "=/="
    | LT -> "<"
    | LEQ -> "<="
    | GT -> ">"
    | GEQ -> ">="
    | ARRAY -> "[]"
    | LBRACKET -> "["
    | RBRACKET -> "]"
    | LPAREN -> "("
    | RPAREN -> ")"
    | LBRACE -> "{"
    | RBRACE -> "}"
    | SEMI -> ";"
    | COMMA -> ","
    | PLUS -> "+"
    | MINUS -> "-"
    | TIMES -> "*"
    | DIVIDE -> "/"
    | MOD -> "%"
    | POWER -> "^"
    | PLUSA -> "+="
    | MINUSA -> "-="
    | TIMESA -> "*="
    | DIVIDEA -> "/="
    | MODA -> "%="
    | POWERA -> "^="
    | IF -> "if"
    | ELSE -> "else"
    | ELSIF -> "elsif"
    | WHILE -> "while"
    | RETURN -> "return"
    | CLASS -> "class"
    | EXTEND -> "extends"
    | SUPER -> "super"
    | INIT -> "init"
    | NULL -> "null"
    | VOID -> "void"
    | THIS -> "this"
    | REFINE -> "refine"
    | REFINES -> "refinement"
    | TO -> "to"
    | PRIVATE -> "private"
    | PUBLIC -> "public"
    | PROTECTED -> "protected"
    | DOT -> "."
    | MAIN -> "main"
    | NEW -> "new"
    | ASSIGN -> ":="
    | ID(var) -> var
    | TYPE(typ) -> typ
    | BLIT(b) -> if b then "true" else "false"
    | ILIT(i) -> string_of_int(i)
    | FLIT(f) -> string_of_float(f)
    | SLIT(s) -> Format.sprintf "\"%s\"" s
    | EOF -> "eof"

(**
    Given a lexing function and a lexing buffer, consume tokesn until
    the end of file is reached. Return the generated tokens.
    @param lexfun A function that takes a lexbuf and returns a token
    @param lexbuf A lexographical buffer from Lexing
    @return A list of scanned tokens
  *)
let token_list (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
    let rec list_tokens rtokens =
        match (lexfun lexbuf) with
            | EOF -> List.rev (EOF::rtokens)
            | tk -> list_tokens (tk::rtokens) in
    list_tokens []

(**
    Scan a list of tokens from an input file.
    @param source A channel to get tokens from
    @return A list of tokens taken from a source
  *)
let from_channel source = token_list Scanner.token (Lexing.from_channel source)

(**
    Print a list of tokens to stdout.
    @param tokens A list of tokens
    @return Only returns a unit
  *)
let print_token_list tokens = print_string (String.concat " " (List.map token_to_string tokens))

(**
    Used to print out de-whitespacing lines which consist of a number (indentation), a list
    of tokens (the line), and whether there is a colon at the end of the line.
    @return Only returns a unit
  *)
let print_token_line = function
    | (space, toks, colon) ->
        print_string ("(" ^ string_of_int space ^ "," ^ string_of_bool colon ^ ") ");
        print_token_list toks

(**
    Print out a list of tokens with a specific header and some extra margins
    @param header A nonsemantic string to preface our list
    @param toks A list of tokens
    @return Only returns a unit
  *)
let pprint_token_list header toks = print_string header ; print_token_list toks ; print_newline ()

(**
    Print out de-whitespacing lines (see print_token_line) for various lines, but with a header.
    @param header A nonsemantic string to preface our list
    @param lines A list of line representations (number of spaces, if it ends in a colon, a list of tokens)
    @return Only returns a unit
  *)
let pprint_token_lines header lines =
    let spaces = String.make (String.length header) ' ' in
    let rec lines_printer prefix = function
        | line::rest ->
            print_string prefix;
            print_token_line line;
            print_newline ();
            lines_printer spaces rest
        | [] -> () in
    lines_printer header lines

(** The majority of the following functions are relatively direct AST to string operations *)

(* Useful for both sAST and AST *)
let _id x = x
let inspect_str_list stringer a_list = Printf.sprintf "[%s]" (String.concat ", " (List.map stringer a_list))
let inspect_opt stringer = function
    | None -> "None"
    | Some(v) -> Printf.sprintf "Some(%s)" (stringer v)

(* AST Parser Stuff *)
let inspect_ast_lit (lit : Ast.lit) = match lit with
    | Int(i)    -> Printf.sprintf "Int(%d)" i
    | Float(f)  -> Printf.sprintf "Float(%f)" f
    | String(s) -> Printf.sprintf "String(\"%s\")" s
    | Bool(b)   -> Printf.sprintf "Bool(%B)" b

let inspect_ast_arith (op : Ast.arith) = match op with
    | Add  -> "Add"
    | Sub  -> "Sub"
    | Prod -> "Prod"
    | Div  -> "Div"
    | Mod  -> "Mod"
    | Neg  -> "Neg"
    | Pow  -> "Pow"

let inspect_ast_numtest (op : Ast.numtest) = match op with
    | Eq   -> "Eq"
    | Neq  -> "Neq"
    | Less -> "Less"
    | Grtr -> "Grtr"
    | Leq  -> "Leq"
    | Geq  -> "Geq"

let inspect_ast_combtest (op : Ast.combtest) = match op with
    | And  -> "And"
    | Or   -> "Or"
    | Nand -> "Nand"
    | Nor  -> "Nor"
    | Xor  -> "Xor"
    | Not  -> "Not"

let inspect_ast_op (op : Ast.op) = match op with
    | Arithmetic(an_op) -> Printf.sprintf "Arithmetic(%s)" (inspect_ast_arith an_op)
    | NumTest(an_op)    -> Printf.sprintf "NumTest(%s)" (inspect_ast_numtest an_op)
    | CombTest(an_op)   -> Printf.sprintf "CombTest(%s)" (inspect_ast_combtest an_op)

let rec inspect_ast_expr (expr : Ast.expr) = match expr with
    | Id(id) -> Printf.sprintf "Id(%s)" id
    | This -> "This"
    | Null -> "Null"
    | NewObj(the_type, args) -> Printf.sprintf("NewObj(%s, %s)") the_type (inspect_str_list inspect_ast_expr args)
    | Anonymous(the_type, args, body) -> Printf.sprintf("Anonymous(%s, %s, %s)") the_type (inspect_str_list inspect_ast_expr args) (inspect_str_list inspect_ast_func_def body)
    | Literal(l) -> Printf.sprintf "Literal(%s)" (inspect_ast_lit l)
    | Invoc(receiver, meth, args) -> Printf.sprintf "Invocation(%s, %s, %s)" (inspect_ast_expr receiver) meth (inspect_str_list inspect_ast_expr args)
    | Field(receiver, field) -> Printf.sprintf "Field(%s, %s)" (inspect_ast_expr receiver) field
    | Deref(var, index) -> Printf.sprintf "Deref(%s, %s)" (inspect_ast_expr var) (inspect_ast_expr var)
    | Unop(an_op, exp) -> Printf.sprintf "Unop(%s, %s)" (inspect_ast_op an_op) (inspect_ast_expr exp)
    | Binop(left, an_op, right) -> Printf.sprintf "Binop(%s, %s, %s)" (inspect_ast_op an_op) (inspect_ast_expr left) (inspect_ast_expr right)
    | Refine(fname, args, totype) -> Printf.sprintf "Refine(%s,%s,%s)" fname (inspect_str_list inspect_ast_expr args) (inspect_opt _id totype)
    | Assign(the_var, the_expr) -> Printf.sprintf "Assign(%s, %s)" (inspect_ast_expr the_var) (inspect_ast_expr the_expr)
    | Refinable(the_var) -> Printf.sprintf "Refinable(%s)" the_var
and inspect_ast_var_def (var : Ast.var_def) = match var with
    | (the_type, the_var) -> Printf.sprintf "(%s, %s)" the_type the_var
and inspect_ast_stmt (stmt : Ast.stmt) = match stmt with
    | Decl(the_def, the_expr) -> Printf.sprintf "Decl(%s, %s)" (inspect_ast_var_def the_def) (inspect_opt inspect_ast_expr the_expr)
    | If(clauses) -> Printf.sprintf "If(%s)" (inspect_str_list inspect_ast_clause clauses)
    | While(pred, body) -> Printf.sprintf "While(%s, %s)" (inspect_ast_expr pred) (inspect_str_list inspect_ast_stmt body)
    | Expr(the_expr) -> Printf.sprintf "Expr(%s)" (inspect_ast_expr the_expr)
    | Return(the_expr) -> Printf.sprintf "Return(%s)" (inspect_opt inspect_ast_expr the_expr)
    | Super(args) -> Printf.sprintf "Super(%s)" (inspect_str_list inspect_ast_expr args)
and inspect_ast_clause ((opt_expr, body) : Ast.expr option * Ast.stmt list) =
    Printf.sprintf "(%s, %s)" (inspect_opt inspect_ast_expr opt_expr) (inspect_str_list inspect_ast_stmt body)
and inspect_ast_class_section (sect : Ast.class_section) = match sect with
    | Publics  -> "Publics"
    | Protects -> "Protects"
    | Privates -> "Privates"
    | Refines  -> "Refines"
    | Mains    -> "Mains"
and inspect_ast_func_def (func : Ast.func_def) =
    Printf.sprintf "{ returns = %s, host = %s, name = %s, static = %B, formals = %s, body = %s, section = %s, inklass = %s, uid = %s }"
    (inspect_opt _id func.returns)
    (inspect_opt _id func.host)
    func.name
    func.static
    (inspect_str_list inspect_ast_var_def func.formals)
    (inspect_str_list inspect_ast_stmt func.body)
    (inspect_ast_class_section func.section)
    func.inklass
    func.uid

let inspect_ast_member_def (mem : Ast.member_def) = match mem with
    | VarMem(vmem) -> Printf.sprintf "VarMem(%s)" (inspect_ast_var_def vmem)
    | MethodMem(mmem) -> Printf.sprintf "MethodMem(%s)" (inspect_ast_func_def mmem)
    | InitMem(imem) -> Printf.sprintf "InitMem(%s)" (inspect_ast_func_def imem)

let inspect_ast_class_sections (sections : Ast.class_sections_def) =
    Printf.sprintf "{ privates = %s, protects = %s, publics = %s, refines = %s, mains = %s }"
    (inspect_str_list inspect_ast_member_def sections.privates)
    (inspect_str_list inspect_ast_member_def sections.protects)
    (inspect_str_list inspect_ast_member_def sections.publics)
    (inspect_str_list inspect_ast_func_def sections.refines)
    (inspect_str_list inspect_ast_func_def sections.mains)

let inspect_ast_class_def (the_klass : Ast.class_def) =
    Printf.sprintf "{ klass = %s, parent = %s, sections = %s }"
    the_klass.klass
    (inspect_opt _id the_klass.parent)
    (inspect_ast_class_sections the_klass.sections)
