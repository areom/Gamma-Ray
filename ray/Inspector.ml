open Parser
open Ast

(* TOKEN stuff *)
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
let from_channel source = token_list Scanner.token (Lexing.from_channel source)

let rec print_token_list tokens = print_string (String.concat " " (List.map token_to_string tokens))

let print_token_line = function
  | (space, toks, colon) ->
    print_string ("(" ^ string_of_int space ^ "," ^ string_of_bool colon ^ ") ");
    print_token_list toks

let pprint_token_list header toks = print_string header ; print_token_list toks ; print_newline ()
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


(* AST Parser Stuff *)
let _id x = x

let inspect_lit = function
  | Int(i)    -> Printf.sprintf "Int(%d)" i
  | Float(f)  -> Printf.sprintf "Float(%f)" f
  | String(s) -> Printf.sprintf "String(%s)" s
  | Bool(b)   -> Printf.sprintf "Bool(%B)" b

let inspect_arith = function
  | Add  -> Printf.sprintf "Add"
  | Sub  -> Printf.sprintf "Sub"
  | Prod -> Printf.sprintf "Prod"
  | Quot -> Printf.sprintf "Quot"
  | Div  -> Printf.sprintf "Div"
  | Mod  -> Printf.sprintf "Mod"
  | Neg  -> Printf.sprintf "Neg"
  | Pow  -> Printf.sprintf "Pow"

let inspect_numtest = function
  | Eq   -> Printf.sprintf "Eq"
  | Neq  -> Printf.sprintf "Neq"
  | Less -> Printf.sprintf "Less"
  | Grtr -> Printf.sprintf "Grtr"
  | Leq  -> Printf.sprintf "Leq"
  | Geq  -> Printf.sprintf "Geq"

let inspect_combtest = function
  | And  -> Printf.sprintf "And"
  | Or   -> Printf.sprintf "Or"
  | Nand -> Printf.sprintf "Nand"
  | Nor  -> Printf.sprintf "Nor"
  | Xor  -> Printf.sprintf "Xor"
  | Not  -> Printf.sprintf "Not"

let inspect_op = function
  | Arithmetic(an_op) -> Printf.sprintf "Arithmetic(%s)" (inspect_arith an_op)
  | NumTest(an_op)    -> Printf.sprintf "NumTest(%s)" (inspect_numtest an_op)
  | CombTest(an_op)   -> Printf.sprintf "CombTest(%s)" (inspect_combtest an_op)

let inspect_str_list stringer a_list = Printf.sprintf "[%s]" (String.concat ", " (List.map stringer a_list))
let inspect_opt stringer = function
  | None -> "None"
  | Some(v) -> Printf.sprintf "Some(%s)" (stringer v)

let rec inspect_expr = function
  | Id(id) -> Printf.sprintf "Id(%s)" id
  | This -> "This"
  | Null -> "Null"
  | NewObj(the_type, args) -> Printf.sprintf("NewObj(%s, %s)") the_type (inspect_str_list inspect_expr args)
  | Anonymous(the_type, args, body) -> Printf.sprintf("Anonymous(%s, %s, %s)") the_type (inspect_str_list inspect_expr args) (inspect_str_list inspect_func_def body)
  | Literal(l) -> Printf.sprintf "Literal(%s)" (inspect_lit l)
  | Invoc(receiver, meth, args) -> Printf.sprintf "Invocation(%s, %s, %s)" (inspect_expr receiver) meth (inspect_str_list inspect_expr args)
  | Field(receiver, field) -> Printf.sprintf "Field(%s, %s)" (inspect_expr receiver) field
  | Deref(var, index) -> Printf.sprintf "Deref(%s, %s)" (inspect_expr var) (inspect_expr var)
  | Unop(an_op, exp) -> Printf.sprintf "Unop(%s, %s)" (inspect_op an_op) (inspect_expr exp)
  | Binop(left, an_op, right) -> Printf.sprintf "Binop(%s, %s, %s)" (inspect_op an_op) (inspect_expr left) (inspect_expr right)
  | Refine(fname, args, totype) -> Printf.sprintf "Refine(%s,%s,%s)" fname (inspect_str_list inspect_expr args) totype
  | Assign(the_var, the_expr) -> Printf.sprintf "Assign(%s, %s)" (inspect_expr the_var) (inspect_expr the_expr)
  | Refinable(the_var) -> Printf.sprintf "Refinable(%s)" the_var
and inspect_var_def (the_type, the_var) = Printf.sprintf "(%s, %s)" the_type the_var
and inspect_stmt = function
  | Decl(the_def, the_expr) -> Printf.sprintf "Decl(%s, %s)" (inspect_var_def the_def) (inspect_opt inspect_expr the_expr)
  | If(clauses) -> Printf.sprintf "If(%s)" (inspect_str_list inspect_clause clauses)
  | While(pred, body) -> Printf.sprintf "While(%s, %s)" (inspect_expr pred) (inspect_str_list inspect_stmt body)
  | Expr(the_expr) -> Printf.sprintf "Expr(%s)" (inspect_expr the_expr)
  | Return(the_expr) -> Printf.sprintf "Return(%s)" (inspect_opt inspect_expr the_expr)
  | Super(args) -> Printf.sprintf "Super(%s)" (inspect_str_list inspect_expr args)
and inspect_clause (opt_expr, body) = Printf.sprintf "(%s, %s)" (inspect_opt inspect_expr opt_expr) (inspect_str_list inspect_stmt body)
and inspect_func_def func = Printf.sprintf "{ returns = %s, host = %s, name = %s, static = %B, formals = %s, body = %s }"
  (inspect_opt _id func.returns)
  (inspect_opt _id func.host)
  func.name
  func.static
  (inspect_str_list inspect_var_def func.formals)
  (inspect_str_list inspect_stmt func.body)

let inspect_member_def = function
  | VarMem(vmem) -> Printf.sprintf "VarMem(%s)" (inspect_var_def vmem)
  | MethodMem(mmem) -> Printf.sprintf "MethodMem(%s)" (inspect_func_def mmem)
  | InitMem(imem) -> Printf.sprintf "InitMem(%s)" (inspect_func_def imem)

let inspect_class_sections sections = Printf.sprintf "{ privates = %s, protects = %s, publics = %s, refines = %s, mains = %s }"
  (inspect_str_list inspect_member_def sections.privates)
  (inspect_str_list inspect_member_def sections.protects)
  (inspect_str_list inspect_member_def sections.publics)
  (inspect_str_list inspect_func_def sections.refines)
  (inspect_str_list inspect_func_def sections.mains)

let inspect_class_def the_klass = Printf.sprintf "{ klass = %s, parent = %s, sections = %s }"
  the_klass.klass
  (inspect_opt _id the_klass.parent)
  (inspect_class_sections the_klass.sections)
