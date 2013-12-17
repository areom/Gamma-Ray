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


and vdecl_to_cstr (vtype, vname) = vtype^" "^vname^";\n"

(**
    Take a list of cast_stmts and return a body of c statements
    @param stmtlist A list of statements
    @return A body of c statements
*)
let rec cast_to_cstmtlist stmtlist = stringify_list  (List.map cast_to_c_stmt stmtlist)

and ifstmt_to_str level (ifexpr, body) = 
    match ifexpr with
    | Some(ifpred)   ->  if level <> 0 then 
       "elseif( " ^ (expr_to_cstr ifpred) ^" ) {\n"
        ^(cast_to_cstmtlist body)^"\n}\n"
        else
        "if( "^(expr_to_cstr ifpred)^" ) {\n"^(cast_to_cstmtlist body)^"\n}\n "
    | None           ->  "else {\n"^(cast_to_cstmtlist body)^" }\n"


(**
    Output a statement in c
    @param caststmt A statement in C-Ast form
    @returm A c statement
*)
and   cast_to_c_stmt caststmt = 

    match caststmt with
      Decl(vdecl, Some(expr), env) -> (vdecl_to_cstr vdecl)^" = "^(expr_to_cstr expr)^";\n"
    | Decl(vdecl, None, env) ->  (vdecl_to_cstr vdecl)^";\n"
    | If(iflist, env) -> stringify_list (List.mapi ifstmt_to_str iflist)
    | While(pred, body, env) -> "while( "^(expr_to_cstr pred)^" ) {\n"^(cast_to_cstmtlist body)^"\n}\n"
    | Expr(expr, env)  -> (expr_to_cstr expr)^";\n"
    | Return(Some(expr), env) -> "return ("^(expr_to_cstr expr)^");\n"
    | _ -> "Yet to implement this statement" 


(**
    Take a C-Ast and return a program in c
    @param cast_cdefs A list of class definitions
    @param cast_cfuncs A list of function defintions
    @param cast_mains A list of main method uids
    @return A program in c
*)
(**
let cast_to_c (cast_cdefs,cast_cfuncs,cast_mains) =
    (cast_to_c_structs cast_cdefs) ^
    (cast_to_c_funcs cast_cfuns) ^
    (cast_to_c_mains cast_mains)*)

(** 
   Take a class definition and return code consisting of struct types 
   @param cast_cdefs A list of class definitions
   @return A set of type definitions
*)
let cast_to_h_types(cast_cdefs) =
    let generic_class_struct =
        "typedef struct{\n"^
        "  int class_data;\n"^
        "  int var_data;\n"^
        "  int refine_data;\n"^
        "} gen_class;\n\n"
    in
    (* I'm a little unsure if there are more data points to add here.
    Maybe this could be replaced with class_name alone? *)
    let class_details_struct =
        "typedef struct {\n" ^
        "  char* class_name;\n" ^
        "} class_details;"
    in
    (**
        Classes have three important component structs: class data,
        refinement data, and variables. They get tied together by a generic
        class struct
    *)
    let cast_to_h_class cast_cdef =
        (** Put out a pointer for each variable as part of a struct *)
        let cast_to_h_var_set =
            "typedef struct {\n" ^
            (String.concat "\n" (List.map
                (fun (type_name,var_name) -> "  gen_class* v_"^var_name^";")
                cast_cdef.variables)) ^
            "} cv_" ^ (List.hd cast_cdef.klass) ^ ";"
        in
        (** Put out a pointer for each refinement function as part of a struct *)
        let cast_to_h_ref_set =
            "typedef struct {\n"^
            (String.concat "\b" (List.map
                (fun ref_name -> "  int f_"^ref_name^";\n")
                cast_cdef.refines)) ^
            "} cr_" ^ (List.hd cast_cdef.klass) ^ ";"
        in
        cast_to_h_var_set ^ "\n\n"
        ^ cast_to_h_ref_set
    in
    generic_class_struct ^ class_details_struct
    ^ (String.concat "\n\n" (List.map cast_to_h_class cast_cdefs))

(**
    Build code for function header definitions
    @param cfuncs A list of function definintions
    @return A string of code containing function header
*)
let cast_to_h_funcs cfuncs =
    let cast_to_h_func cfunc =
        let cast_to_h_var (type_name,var_name) =
            "gen_class * v_"^var_name
        in
        "gen_class * f_"^cfunc.uid^"(gen_class * host,"
        ^(String.concat "," (List.map cast_to_h_var cfunc.formals))
        ^");"
    in
    (String.concat "\n\n" (List.map cast_to_h_func cfuncs))
