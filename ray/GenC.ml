open Cast

let c_indent = "  "

let lit_to_str lit =
    match lit with
      Ast.Int(i) -> string_of_int i
    | Ast.Float(f) -> string_of_float f
    | Ast.String(s) -> "\"" ^ s ^ "\""  (* escapes were escaped during lexing *)
    | Ast.Bool(b) ->if b then "1" else "0"


let stringify_unop op rop =
    match op with
      Ast.Arithmetic(Ast.Neg) -> "-"^rop
    | Ast.CombTest(Ast.Not)   -> "!"^rop
    | _   -> raise (Failure "Unknown operator")

let stringify_arith op lop rop =
    match op with
      Ast.Add  -> lop^" + "^rop
    | Ast.Sub  -> lop^" - "^rop
    | Ast.Prod -> lop^" * "^rop
    | Ast.Div  -> lop^" / "^rop
    | Ast.Mod  -> lop^" % "^rop
    | Ast.Neg  ->  raise(Failure "Unary operator")
    | Ast.Pow  -> Format.sprintf "pow(%s,%s)" lop rop

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

let stringify_binop op lop rop=
    match op with
      Ast.Arithmetic(arith)  -> stringify_arith arith lop rop
    | Ast.NumTest(numtest)   -> stringify_numtest numtest lop rop
    | Ast.CombTest(combtest) -> stringify_combtest combtest lop rop

let stringify_list stmtlist =
   String.concat "\n" stmtlist

let rec expr_to_cstr (exptype, expr_detail) = exprdetail_to_cstr expr_detail

and exprdetail_to_cstr castexpr_detail =
    let generate_deref obj index =
        Format.sprintf "((%s *)(%s))[(%s)]" (GenCast.get_tname "Object") (expr_to_cstr obj) (expr_to_cstr index) in

    let generate_field obj field =
        (* Put it off until later, via MACROS! *)
        Format.sprintf "DEREF(%s, %s)" (expr_to_cstr obj) field in

    let generate_invocation recvr fname args =
        let this = expr_to_cstr recvr in
        let vals = List.map expr_to_cstr args in
        match args with
            | [] -> Format.sprintf "%s(%s)" fname this
            | args -> Format.sprintf "%s(%s, %s)" fname this (String.concat ", " vals) in

    let get_vreference vname = function
        | Sast.Local -> vname
        | Sast.Instance(_) -> "this->" ^ vname in

    match castexpr_detail with
    | This                           -> "this" (* There is no way this is right with implicit object passing *)
    | Null                           -> "NULL"
    | Id(vname, varkind)             -> get_vreference(vname, varkind)
    | NewObj(classname, fname, args) -> allocate_obj classname fname args
    | Literal(lit)                   -> lit_to_str lit
    | Assign(memory, data)           -> (expr_to_cstr memory)^" = "^(expr_to_cstr data)
    | Deref(carray, index)           -> generate_deref carray index
    | Field(obj, fieldname)          -> generate_field obj fieldname
    | Invoc(recvr, fname, args)      -> generate_invocation rcvr fname args
    | Unop(op, expr)                 -> stringify_unop op (expr_to_cstr expr)
    | Binop(lop, op, rop)            -> "( "^(stringify_binop op (expr_to_cstr lop) (expr_to_cstr rop))^" )"

and vdecl_to_cstr (vtype, vname) = vtype ^ " " ^ vname

(**
    Take a list of cast_stmts and return a body of c statements
    @param stmtlist A list of statements
    @return A body of c statements
*)
let rec cast_to_cstmtlist stmtlist = stringify_list  (List.map cast_to_c_stmt stmtlist)

and ifstmt_to_str level (ifexpr, body) =
    let cbody = Format.sprintf "{\n %s \n}" (cast_to_cstmtlist body) in
    match ifexpr with
    | Some(ifpred) ->
        let thisif = Format.sprintf "if ( %s ) %s\n" (expr_to_cstr ifpred) cbody in
        if level = 0 then thisif else "else " ^ thisif
    | None -> Format.sprintf "else %s\n" cbody

(**
    Output a statement in c
    @param caststmt A statement in C-Ast form
    @returm A c statement
*)
and cast_to_c_stmt caststmt =
    let c_stmt =
        match caststmt with
        | Decl(vdecl, Some(expr), env) -> Format.sprintf "%s = (%s);\n" (vdecl_to_cstr vdecl) (expr_to_cstr expr)
        | Decl(vdecl, None, env) -> Format.sprintf "%s;\n" (vdecl_to_cstr vdecl)
        | If(iflist, env) -> stringify_list (List.mapi ifstmt_to_str iflist)
        | While(pred, body, env) -> Format.sprintf "while ( %s ) {\n %s \n}\n" (expr_to_cstr pred) (cast_to_cstmtlist body)
        | Expr(expr, env) -> Format.sprintf "( %s );\n" (expr_to_cstr expr)
        | Return(Some(expr), env) -> "return ( %s );\n" (expr_to_cstr expr)
        | _ ->
            "Yet to implement this statement"
    in
    c_indent ^ c_stmt

(**
    Define class objects for our classes. The heavy lifting is done in the header.
    @param cast_cdefs A list of class definitions
    @return Code for instantiating class stuff
*)
let cast_to_c_classes cast_cdefs =
    let cast_to_c_class cast_cdef =
        let class_name = (List.hd cast_cdef.klass) in
        (** Initialize a struct object for this class *)
        class_name ^ "_class_details = {" ^ class_name ^ "};"
    in
    (String.concat "\n" (List.map cast_to_c_class cast_cdefs))

(** Build a function signature *)
let build_sig cfunc =
    let build_formal (type_name,var_name) =
        "gen_class * v_"^var_name
    in
    let possible_host = if cfunc.static then ""
        else "gen_class * host,"
    in
    "gen_class * f_" ^ cfunc.uid ^ "(" ^ possible_host
    ^(String.concat "," (List.map build_formal cfunc.formals))
    ^")"

(** Remove builtins from a func list *)
let strip_builtins cfuncs = List.filter (fun cfunc -> not cfunc.builtin) cfuncs

(**
    Define our functions. The heavy lifting is done in the header.
    @param cfuncs A list of function definitions
    @return Code for instantiating class stuff
*)
let cast_to_c_funcs cfuncs =
    let cast_to_c_func cfunc =
        (build_sig cfunc) ^ "{\n" ^
        (cast_to_cstmtlist cfunc.body)
        ^ "\n}"
    in
    (String.concat "\n" (List.map cast_to_c_func (strip_builtins cfuncs)))

(**
    Build the choice of mains. (I'd rather spend time on
    checking then implementing main selection. Sorry!)
    @param cast_cdefs A list of class mains
    @return A c main function calling our main function.
*)
let cast_to_c_mains cast_mains =
    let first_main = List.hd cast_mains in
    "int main(int argc, char **argv){\n" ^
    c_indent ^ "f_" ^ first_main ^ "();\n" ^
    "}"

(**
    Take a C-Ast and return a program in c
    @param cast_cdefs A list of class definitions
    @param cast_cfuncs A list of function defintions
    @param cast_mains A list of main method uids
    @return A program in c
*)
let cast_to_c (cast_cdefs,cast_cfuncs,cast_mains) =
    (cast_to_c_classes cast_cdefs) ^
    (cast_to_c_funcs cast_cfuncs) ^
    (cast_to_c_mains cast_mains)

(**
   Take a class definition and return code consisting of struct types
   @param cast_cdefs A list of class definitions
   @return A set of type definitions
*)
let cast_to_h_classes cast_cdefs =
    let generic_class_struct =
        "typedef struct{\n"^
        c_indent ^ "int class_data;\n"^
        c_indent ^ "int var_data;\n"^
        c_indent ^ "int refine_data;\n"^
        "} gen_class;\n\n"
    in
    (* I'm a little unsure if there are more data points to add here.
    Maybe this could be replaced with class_name alone? *)
    let class_details_struct =
        "typedef struct {\n" ^
        c_indent ^ "char* class_name;\n" ^
        "} class_details;"
    in
    (**
        Classes have three important component structs: class data,
        refinement data, and variables. They get tied together by a generic
        class struct
    *)
    let cast_to_h_class cast_cdef =
        let class_name = (List.hd cast_cdef.klass) in
        (** Define a struct object for this class *)
        let cast_to_h_class_detail =
            "class_details " ^ class_name ^ "_class_details;"
        in
        (** Put out a pointer for each variable as part of a struct *)
        let cast_to_h_var_set =
            "typedef struct {\n" ^
            (String.concat "\n" (List.map
                (fun (type_name,var_name) -> c_indent ^ "gen_class* v_"^var_name^";")
                cast_cdef.variables)) ^
            "} cv_" ^ class_name ^ ";"
        in
        (** Put out a pointer for each refinement function as part of a struct *)
        let cast_to_h_ref_set =
            "typedef struct {\n"^
            (String.concat "\b" (List.map
                (fun ref_name -> c_indent ^ "int f_"^ref_name^";\n")
                cast_cdef.refines)) ^
            "} cr_" ^ class_name ^ ";" ^
            (** Pre-define the refinement object. Name is sorta stupid *)
            "cr_" ^ class_name ^ " " ^ class_name ^ "_refines;"
        in
        cast_to_h_class_detail ^ "\n\n"
        ^ cast_to_h_var_set ^ "\n\n"
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
    (String.concat "\n\n"
        (List.map (fun cfunc -> (build_sig cfunc) ^ ";") (strip_builtins cfuncs)))

(**
    Take a C-Ast and return a header in c
    @param cast_cdefs A list of class definitions
    @param cast_cfuncs A list of function defintions
    @return A header in c
*)
let cast_to_h (cast_cdefs,cast_cfuncs,cast_mains) =
    (cast_to_h_classes cast_cdefs) ^
    (cast_to_h_funcs cast_cfuncs)
