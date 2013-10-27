open Ast

let inspect_lit lit = match lit with
  | Int(i)    -> Printf.sprintf "Int(%d)" i
  | Float(f)  -> Printf.sprintf "Float(%f)" f
  | String(s) -> Printf.sprintf "String(%s)" s
  | Bool(b)   -> Printf.sprintf "Bool(%B)" b

let inspect_arith the_op = match the_op with
  | Add  -> Printf.sprintf "Add"
  | Sub  -> Printf.sprintf "Sub"
  | Prod -> Printf.sprintf "Prod"
  | Quot -> Printf.sprintf "Quot"
  | Div  -> Printf.sprintf "Div"
  | Mod  -> Printf.sprintf "Mod"
  | Neg  -> Printf.sprintf "Neg"
  | Pow  -> Printf.sprintf "Pow"

let inspect_numtest the_op = match the_op with
  | Eq   -> Printf.sprintf "Eq"
  | Neq  -> Printf.sprintf "Neq"
  | Less -> Printf.sprintf "Less"
  | Grtr -> Printf.sprintf "Grtr"
  | Leq  -> Printf.sprintf "Leq"
  | Geq  -> Printf.sprintf "Geq"

let inspect_combtest the_op = match the_op with
  | And  -> Printf.sprintf "And"
  | Or   -> Printf.sprintf "Or"
  | Nand -> Printf.sprintf "Nand"
  | Nor  -> Printf.sprintf "Nor"
  | Xor  -> Printf.sprintf "Xor"
  | Not  -> Printf.sprintf "Not"

let inspect_op the_op = match the_op with
  | Arithmetic(an_op) -> Printf.sprintf "Arithmetic(%s)" (inspect_arith an_op)
  | NumTest(an_op)    -> Printf.sprintf "NumTest(%s)" (inspect_numtest an_op)
  | CombTest(an_op)   -> Printf.sprintf "CombTest(%s)" (inspect_combtest an_op)

let inspect_str_list stringer a_list = Printf.sprintf "[%s]" (String.concat ", " (List.map stringer a_list))
let inspect_opt stringer opt = match opt with
  | None -> "None"
  | Some(v) -> Printf.sprintf "Some(%s)" (stringer v)

let rec inspect_expr the_expr = match the_expr with
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
and inspect_stmt the_stmt = match the_stmt with
  | Decl(the_def, the_expr) -> Printf.sprintf "Decl(%s, %s)" (inspect_var_def the_def) (inspect_opt inspect_expr the_expr)
  | If(clauses) -> Printf.sprintf "If(%s)" (inspect_str_list inspect_clause clauses)
  | While(pred, body) -> Printf.sprintf "While(%s, %s)" (inspect_expr pred) (inspect_str_list inspect_stmt body)
  | Expr(the_expr) -> Printf.sprintf "Expr(%s)" (inspect_expr the_expr)
  | Return(the_expr) -> Printf.sprintf "Return(%s)" (inspect_expr the_expr)
  | Super(args) -> Printf.sprintf "Super(%s)" (inspect_str_list inspect_expr args)
and inspect_clause (opt_expr, body) = Printf.sprintf "(%s, %s)" (inspect_opt inspect_expr opt_expr) (inspect_str_list inspect_stmt body)
and inspect_func_def func = let id x = x in Printf.sprintf "{ returns = %s, host = %s, name = %s, static = %B, formals = %s, body = %s }"
  (inspect_opt id func.returns)
  (inspect_opt id func.host)
  func.name
  func.static
  (inspect_str_list inspect_var_def func.formals)
  (inspect_str_list inspect_stmt func.body)

let inspect_member_def mem = match mem with
  | VarMem(vmem) -> Printf.sprintf "VarMem(%s)" (inspect_var_def vmem)
  | MethodMem(mmem) -> Printf.sprintf "MethodMem(%s)" (inspect_func_def mmem)
  | InitMem(imem) -> Printf.sprintf "InitMem(%s)" (inspect_func_def imem)

let inspect_class_sections sections = Printf.sprintf "{ privates = %s, protects = %s, publics = %s, refines = %s, mains = %s }"
  (inspect_str_list inspect_member_def sections.privates)
  (inspect_str_list inspect_member_def sections.protects)
  (inspect_str_list inspect_member_def sections.publics)
  (inspect_str_list inspect_func_def sections.refines)
  (inspect_str_list inspect_func_def sections.mains)

let inspect_class_def the_klass = let id x = x in Printf.sprintf "{ klass = %s, parent = %s, sections = %s }"
  the_klass.klass
  (inspect_opt id the_klass.parent)
  (inspect_class_sections the_klass.sections)

let get_classes lexer = Parser.cdecls Scanner.token lexer

let read_classes where = get_classes (Lexing.from_channel where)

let inspect_classes source = List.map inspect_class_def (read_classes source)

let _ = print_string (String.concat "\n\n" (inspect_classes stdin)) ; print_newline()

