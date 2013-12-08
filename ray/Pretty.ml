open Parser
open Ast

(** A collection of pretty printing functions.
    I don't believe it actually needs the Parser dependency.
    Should probably absorb a fair margin from other files like Inspector.ml *)

let indent level = String.make (level*2) ' '
let _id x = x

let pp_lit = function
  | Int(i)    -> Printf.sprintf "Int(%d)" i
  | Float(f)  -> Printf.sprintf "Float(%f)" f
  | String(s) -> Printf.sprintf "String(%s)" s
  | Bool(b)   -> Printf.sprintf "Bool(%B)" b

let pp_arith = function
  | Add  -> "Add"
  | Sub  -> "Sub"
  | Prod -> "Prod"
  | Quot -> "Quot"
  | Div  -> "Div"
  | Mod  -> "Mod"
  | Neg  -> "Neg"
  | Pow  -> "Pow"

let pp_numtest = function
  | Eq   -> "Eq"
  | Neq  -> "Neq"
  | Less -> "Less"
  | Grtr -> "Grtr"
  | Leq  -> "Leq"
  | Geq  -> "Geq"

let pp_combtest = function
  | And  -> "And"
  | Or   -> "Or"
  | Nand -> "Nand"
  | Nor  -> "Nor"
  | Xor  -> "Xor"
  | Not  -> "Not"

let pp_op = function
  | Arithmetic(an_op) -> Printf.sprintf "Arithmetic(%s)" (pp_arith an_op)
  | NumTest(an_op)    -> Printf.sprintf "NumTest(%s)" (pp_numtest an_op)
  | CombTest(an_op)   -> Printf.sprintf "CombTest(%s)" (pp_combtest an_op)

let pp_str_list stringer a_list depth = Printf.sprintf "[ %s ]" (String.concat ", " (List.map stringer a_list))
let pp_opt stringer = function
  | None -> "None"
  | Some(v) -> Printf.sprintf "Some(%s)" (stringer v)

let rec pp_expr depth = function
  | Id(id) -> Printf.sprintf "Id(%s)" id
  | This -> "This"
  | Null -> "Null"
  | NewObj(the_type, args) -> Printf.sprintf("\n%sNewObj(%s, %s)") (indent depth) the_type (pp_str_list (pp_expr depth) args depth)
  | Anonymous(the_type, args, body) -> Printf.sprintf("\n%sAnonymous(%s, %s, %s)") (indent depth) the_type (pp_str_list (pp_expr depth) args depth) (pp_str_list (pp_func_def depth) body depth)
  | Literal(l) -> Printf.sprintf "\n%sLiteral(%s)" (indent depth) (pp_lit l)
  | Invoc(receiver, meth, args) -> Printf.sprintf "\n%sInvocation(%s, %s, %s)" (indent depth) ((pp_expr (depth+1)) receiver) meth (pp_str_list (pp_expr (depth+1)) args depth)
  | Field(receiver, field) -> Printf.sprintf "\n%sField(%s, %s)" (indent depth) ((pp_expr depth) receiver) field
  | Deref(var, index) -> Printf.sprintf "\n%sDeref(%s, %s)" (indent depth) ((pp_expr depth) var) ((pp_expr depth) var)
  | Unop(an_op, exp) -> Printf.sprintf "\n%sUnop(%s, %s)" (indent depth) (pp_op an_op) ((pp_expr depth) exp)
  | Binop(left, an_op, right) -> Printf.sprintf "\n%sBinop(%s, %s, %s)" (indent depth) (pp_op an_op) ((pp_expr depth) left) ((pp_expr depth) right)
  | Refine(fname, args, totype) -> Printf.sprintf "Refine(%s, %s, %s)" fname (pp_str_list (pp_expr (depth+1)) args (depth+1)) (pp_opt _id  totype)
  | Assign(the_var, the_expr) -> Printf.sprintf "\n%sAssign(%s, %s)" (indent depth) ((pp_expr (depth+1)) the_var) ((pp_expr (depth+1)) the_expr)
  | Refinable(the_var) -> Printf.sprintf "\n%sRefinable(%s)" (indent depth) the_var
and pp_var_def depth (the_type, the_var) = Printf.sprintf "\n%s(%s, %s)" (indent depth) the_type the_var
and pp_stmt depth = function
  | Decl(the_def, the_expr) -> Printf.sprintf "\n%sDecl(%s, %s)" (indent depth) ((pp_var_def (depth+1)) the_def) (pp_opt (pp_expr depth) the_expr)
  | If(clauses) -> Printf.sprintf "\n%sIf(%s)" (indent depth) (pp_str_list (inspect_clause depth) clauses depth)
  | While(pred, body) -> Printf.sprintf "\n%sWhile(%s, %s)" (indent depth) ((pp_expr depth) pred) (pp_str_list (pp_stmt (depth+1)) body depth)
  | Expr(the_expr) -> Printf.sprintf "\n%sExpr(%s)" (indent depth) ((pp_expr (depth+1)) the_expr)
  | Return(the_expr) -> Printf.sprintf "\n%sReturn(%s)" (indent depth) (pp_opt (pp_expr depth) the_expr)
  | Super(args) -> Printf.sprintf "\n%sSuper(%s)" (indent depth) (pp_str_list (pp_expr depth) args depth)
and inspect_clause depth (opt_expr, body) = Printf.sprintf "(%s, %s)" (pp_opt (pp_expr depth) opt_expr) (pp_str_list (pp_stmt (depth+1)) body depth)
and class_section = function
  | Publics  -> "Publics"
  | Protects -> "Protects"
  | Privates -> "Privates"
  | Refines  -> "Refines"
  | Mains    -> "Mains"
and pp_func_def depth func = Printf.sprintf "\n%s{\n%sreturns = %s,\n%shost = %s,\n%sname = %s,\n%sstatic = %B,\n%sformals = %s,\n%sbody = %s,\n%ssection = %s,\n%sinklass = %s\n%s}"
  (indent (depth-1))
  (indent depth)
  (pp_opt _id func.returns)
  (indent depth)
  (pp_opt _id func.host)
  (indent depth)
  func.name
  (indent depth)
  func.static
  (indent depth)
  (pp_str_list (pp_var_def (depth+1)) func.formals depth)
  (indent depth)
  (pp_str_list (pp_stmt (depth+1)) func.body depth)
  (indent depth)
  (class_section func.section)
  (indent depth)
  func.inklass
  (indent (depth-1))

let pp_member_def depth = function
  | VarMem(vmem) -> Printf.sprintf "\n%sVarMem(%s)" (indent depth) (pp_var_def (depth+1) vmem)
  | MethodMem(mmem) -> Printf.sprintf "\n%sMethodMem(%s)" (indent depth) (pp_func_def (depth+1) mmem)
  | InitMem(imem) ->  (*let fmt = "@[<v " ^^ (string_of_int depth) ^^ ">@,InitMem(%s)@]" in*)
    Format.sprintf "\n%sInitMem(%s)@]"
    (indent depth) (pp_func_def (depth+1) imem)
    (*Format.sprintf fmt
    (pp_func_def (depth+1) imem)*)

let pp_class_sections sections depth =
  Format.sprintf "@[<v 3>@,{@[<v 2>@,privates = %s,@,protects = %s,@,publics = %s,@,refines = %s,@,mains = %s@]@,}@]"
  (pp_str_list (pp_member_def (depth+1)) sections.privates depth)
  (pp_str_list (pp_member_def (depth+1)) sections.protects depth)
  (pp_str_list (pp_member_def (depth+1)) sections.publics depth)
  (pp_str_list (pp_func_def (depth+1)) sections.refines depth)
  (pp_str_list (pp_func_def (depth+1)) sections.mains depth)

let pp_class_def the_klass =
  Format.sprintf "@[<v>@,{@[<v 2>@,klass = %s,@,parent = %s,@,sections = %s@]@,}@]"
  the_klass.klass
  (pp_opt _id the_klass.parent)
  (pp_class_sections the_klass.sections 3)
