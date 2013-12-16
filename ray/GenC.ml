open Cast

let lit_to_str lit = 
    match lit with
      Ast.Int(i) -> string_of_int i
    | Ast.Float(f) -> string_of_float f
    | Ast.String(s) -> s
    | Ast.Bool(b) ->if b = true then "1" else "0"


let stringify_unop op lop = 
    match op with
      Ast.Arithmetic(Ast.Neg) -> "-"^lop
    | Ast.CombTest(Ast.Not)   -> "!"^lop
    | _   -> raise (Failure "Unknown operator")

let stringify_arith op lop rop =
    match op with
      Ast.Add  -> lop^" + "^rop
    | Ast.Sub  -> lop^" - "^rop
    | Ast.Prod -> lop^" * "^rop
    | Ast.Div  -> lop^" / "^rop
    | Ast.Mod  -> lop^" % "^rop
    | Ast.Neg  ->  raise(Failure "Unary operator")
    | Ast.Pow  -> lop^" ^ "^rop
    | Ast.Quot ->  raise(Failure "Unused Quot remove it")

let stringify_numtest op lop rop =
    match op with
      Ast.Eq   -> lop^" == "^rop
    | Ast.Neq  -> lop^" != "^rop
    | Ast.Less -> lop^" < "^rop
    | Ast.Grtr -> lop^" > "^rop
    | Ast.Leq  -> lop^" <= "^rop
    | Ast.Geq  -> lop^" >= "^rop

let stringify_combtest op lop rop =
    match op with
      Ast.And  -> lop^" && "^rop
    | Ast.Or   -> lop^" || "^rop
    | Ast.Nand -> "!( "^lop^" && "^rop^" )"
    | Ast.Nor  -> "!( "^lop^" || "^rop^" )"
    | Ast.Xor  -> "!( "^lop^" == "^rop^" )" 
    | Ast.Not  -> raise(Failure "Unary operator")

let stringify_binop op  lop rop=
    match op with
      Ast.Arithmetic(arith)  -> stringify_arith arith lop rop
    | Ast.NumTest(numtest)   -> stringify_numtest numtest lop rop
    | Ast.CombTest(combtest) -> stringify_combtest combtest lop rop

let stringify_list stmtlist =
   String.concat "\n" stmtlist

let rec expr_to_cstr (exptype, expr_detail) = exprdetail_to_cstr expr_detail

and exprdetail_to_cstr castexpr_detail =

    match castexpr_detail with
    | This          -> "this"
    | Null          -> "NULL"
    | Id(vname)     -> vname
    | NewObj(classname, fname, args) -> "NEWOBJ TBD"
    | Literal(lit)  -> lit_to_str lit
    | Assign(memory, data) -> (expr_to_cstr memory)^" = "^(expr_to_cstr data)
    | Deref(carray, index) -> (expr_to_cstr carray)^"["^(expr_to_cstr index)^"]"
    | Field(obj, fieldname) -> (expr_to_cstr obj)^"."^(fieldname) 
    | Invoc(recvr, fname, args) ->  "IMPLEMENT INVOC - TBD"
    | Unop(op, expr) -> stringify_unop op (expr_to_cstr expr) 
    | Binop(lop, op, rop) -> "( "^(stringify_binop op (expr_to_cstr lop) (expr_to_cstr rop))^" )"

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



