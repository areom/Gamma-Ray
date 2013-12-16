open Cast

let rec cast_to_c_expr cast_expr =
    c_expr castexpr

and cast_to_c_exprlist cexprlist = List.map cast_to_c_expr cexprlist

and c_expr castexpr
    match castexpr with
    | This          ->
    | Null          -> "NULL"
    | Id(vname)     -> vname
    | NewObj(classname, fname, args) ->
    | Literal(lit)  -> lit_to_str lit
    | Assign(memory, data) -> (expr_to_str memory)^" = "^(expr_to_str data)
    | Deref(carray, index) -> (expr_to_str carray)^"["^(expr_to_str index)
    | Field(obj, fieldname) -> (expr_to_str obj)^"."^(fieldname) 
    | Invoc(recvr, fname, args) -> 

let cast_to_c_stmt caststmt = 
    match caststmt with
    | Decl
    | If
    | While
    | Expr
    | Return
    | Super

let cast_to_c_class castclass =
