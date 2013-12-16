open Cast
 
let stringify_arith op lop rop =
    match op with
      Add  -> " + "
    | Sub  -> " - "
    | Prod -> " * "
    | Div  -> " / "
    | Mod  -> " % "
    | Neg  -> " may be wrong - ? it's an unop"
    | Pow  -> " ^ "

let stringify_numtest op lop rop =
    match op with
      Eq   -> " == "
    | Neq  -> " != "
    | Less -> " < "
    | Grtr -> " > "
    | Leq  -> " <= "
    | Geq  -> " >= "

let stringify_combtest op lop rop =
    match combtest with
      And  -> " && "
    | Or   -> " || "
    | Nand -> ""

let stringify_binop op  lop rop=
    match op with
      Arithmetic(arith)  -> stringify_arith arith lop rop
    | NumTest(numtest)   -> stringify_numtest numtest lop rop
    | CombTest(combtest) -> stringify_combtest combtest lop rop

let stringify_list stmtlist =
   String.concat "\n" stmtlist

let rec expr_to_cstr (exptype, expr_detail) = exprdetail_to_cstr expr_detail

and exprdetail_to_cstr castexpr_detail =

    match castexpr_detail with
    | This          -> "We wont have a this right?"
    | Null          -> "NULL"
    | Id(vname)     -> vname
    | NewObj(classname, fname, args) -> " "
    | Literal(lit)  -> "lit_to_str lit"
    | Assign(memory, data) -> (expr_to_cstr memory)^" = "^(expr_to_cstr data)
    | Deref(carray, index) -> (expr_to_cstr carray)^"["^(expr_to_cstr index)^"]"
    | Field(obj, fieldname) -> (expr_to_cstr obj)^"."^(fieldname) 
    | Invoc(recvr, fname, args) ->  " Invoc"
    | Unop(op, expr) -> "Unop" 
    | Binop(lop, op, rop) -> stringify_binop op (expr_to_cstr lop) (expr_to_cstr rop)

let rec cast_to_cstmtlist stmtlist = stringify_list  (List.map cast_to_c_stmt stmtlist)

and vdecl_to_cstr (vtype, vname) = vtype^" "^vname^";\n"

and ifstmt_to_str level (ifexpr, body) = 

    match ifexpr with
      Some(ifpred) ->  if level <> 0 then 
                           "elseif( "^(expr_to_cstr ifpred)^" ) {\n"^(cast_to_cstmtlist body)^"\n}\n"
                       else
                           "if( "^(expr_to_cstr ifpred)^" ) {\n"^(cast_to_cstmtlist body)^"\n}\n "

    | None   ->       "else {\n"^(cast_to_cstmtlist body)^" }\n"


and   cast_to_c_stmt caststmt = 

    match caststmt with
      Decl(vdecl, Some(expr), env) -> (vdecl_to_cstr vdecl)^" = "^(expr_to_cstr expr)^";\n"
    | Decl(vdecl, None, env) ->  (vdecl_to_cstr vdecl)^";\n"
    | If(iflist, env) -> stringify_list (List.mapi ifstmt_to_str iflist)
    | While(pred, body, env) -> "while( "^(expr_to_cstr pred)^" ) {\n"^(cast_to_cstmtlist body)^"\n}\n"
    | Expr(expr, env)  -> (expr_to_cstr expr)^";\n"
    | Return(Some(expr), env) -> "return ("^(expr_to_cstr expr)^");\n"
    | _ -> "Yet to implement this statement" 



