open Cast
open StringModules

let c_indent = "  "

let dispatches = ref []
let dispatchon = ref []

let matches type1 type2 = (GenCast.get_tname type1) = type2

let lit_to_str lit = match lit with
    | Ast.Int(i) -> "LIT_INT("^(string_of_int i)^")"
    | Ast.Float(f) -> "LIT_FLOAT("^(string_of_float f)^")"
    | Ast.String(s) -> "LIT_STRING(\"" ^ s ^ "\")"  (* escapes were escaped during lexing *)
    | Ast.Bool(b) ->if b then "LIT_BOOL(1)" else "LIT_BOOL(0)"

let stringify_unop op rop rtype =
    let (is_int, is_flt, is_bool) = (matches "Integer", matches "Float", matches "Boolean") in
    let is_type = (is_int rtype, is_flt rtype, is_bool rtype) in
    let type_capital = match is_type with
        | (true, _, _) -> "INTEGER"
        | (_, true, _) -> "FLOAT"
        | (_, _, true) -> "BOOLEAN"
        | (_, _, _)    -> raise(Failure "Imcompatible type with unop") in
    match op with
    | Ast.Arithmetic(Ast.Neg) -> "NEG_"^type_capital^"( "^rop^" )"
    | Ast.CombTest(Ast.Not)   -> "NOT_"^type_capital^"( "^rop^" )"
    | _   -> raise (Failure "Unknown operator")

let stringify_arith op suffix =
    match op with
    | Ast.Add  -> "ADD_"^suffix
    | Ast.Sub  -> "SUB_"^suffix
    | Ast.Prod -> "PROD_"^suffix
    | Ast.Div  -> "DIV_"^suffix
    | Ast.Mod  -> "MOD_"^suffix
    | Ast.Neg  ->  raise(Failure "Unary operator")
    | Ast.Pow  -> "POW_"^suffix
  (*| Ast.Pow  -> Format.sprintf "pow(%s,%s)" lop rop*)

let stringify_numtest op suffix = match op with
    | Ast.Eq   -> "NTEST_EQ_"^suffix
    | Ast.Neq  -> "NTEST_NEQ_"^suffix
    | Ast.Less -> "NTEST_LESS_"^suffix
    | Ast.Grtr -> "NTEST_GRTR_"^suffix
    | Ast.Leq  -> "NTEST_LEQ_"^suffix
    | Ast.Geq  -> "NTEST_GEQ_"^suffix

let stringify_combtest op suffix = match op with
    | Ast.And  -> "CTEST_AND_"^suffix
    | Ast.Or   -> "CTEST_OR_"^suffix
    | Ast.Nand -> "CTEST_NAND_"^suffix
    | Ast.Nor  -> "CTEST_NOR_"^suffix
    | Ast.Xor  -> "CTEST_XOR_"^suffix
    | Ast.Not  -> raise(Failure "Unary operator")

let stringify_binop op lop rop types =
    let (is_int, is_flt, is_bool) = (matches "Integer", matches "Float", matches "Boolean") in
    let is_type = (is_int (fst types), is_flt (fst types), is_bool (fst types), is_int (snd types), is_flt (snd types), is_bool (snd types)) in
    let prefix = match is_type with
        | (true, _, _, true, _, _) -> "INT_INT"
        | (_, true, _, _, true, _) -> "FLOAT_FLOAT"
        | (true, _, _, _, true, _) -> "INT_FLOAT"
        | (_, true, _, true, _, _) -> "FLOAT_INT"
        | (_, _, true, _, _, true) -> "BOOL_BOOL"
        | (_, _, _, _, _, _)       -> raise(Failure(Format.sprintf "Binary operator applied to %s, %s" (fst types) (snd types))) in
    let suffix = prefix^"( "^lop^" , "^rop^" )" in
    match op with
    | Ast.Arithmetic(arith)  -> stringify_arith arith suffix
    | Ast.NumTest(numtest)   -> stringify_numtest numtest suffix
    | Ast.CombTest(combtest) -> stringify_combtest combtest suffix

let stringify_list stmtlist = String.concat "\n" stmtlist

let rec expr_to_cstr (exptype, expr_detail) = exprdetail_to_cstr expr_detail

and exprdetail_to_cstr castexpr_detail =
    let generate_deref obj index =
        Format.sprintf "((%s *)(%s))[(%s)]" (GenCast.get_tname "Object") (expr_to_cstr obj) (expr_to_cstr index) in

    let generate_field obj field =
        Format.sprintf "(%s)->%s" (expr_to_cstr obj) field in

    let generate_invocation recvr fname args =
        let this = expr_to_cstr recvr in
        let vals = List.map expr_to_cstr args in
        match args with
            | [] -> Format.sprintf "%s(%s)" fname this
            | args -> Format.sprintf "%s(%s, %s)" fname this (String.concat ", " vals) in

    let generate_vreference vname = function
        | Sast.Local -> vname
        | Sast.Instance(_) -> "this->" ^ vname in

    let generate_allocation klass fname args =
        let vals = List.map expr_to_cstr args in
        match args with
            | [] -> Format.sprintf "%s(MakeNew(%s))" fname klass
            | _ -> Format.sprintf "%s(MakeNew(%s), %s)" fname klass (String.concat ", " vals) in

    let generate_refine args ret = function
        | Sast.Switch(_, _, dispatch) ->
          let vals = List.map expr_to_cstr args in
          (match args with
              | [] -> Format.sprintf "%s(this)" dispatch
              | _ -> Format.sprintf "%s(this, %s)" dispatch (String.concat ", " vals))
        | _ -> raise(Failure("Wrong switch applied to refine -- compiler error.")) in
    let generate_refinable = function
        | Sast.Test(_, _, dispatchby) -> Format.sprintf "%s(this)" dispatchby
        | _ -> raise(Failure("Wrong switch applied to refinable -- compiler error.")) in

    match castexpr_detail with
    | This                           -> "this" (* There is no way this is right with implicit object passing *)
    | Null                           -> "NULL"
    | Id(vname, varkind)             -> generate_vreference vname varkind
    | NewObj(classname, fname, args) -> generate_allocation classname fname args
    | Literal(lit)                   -> lit_to_str lit
    | Assign(memory, data)           -> (expr_to_cstr memory)^" = "^(expr_to_cstr data)
    | Deref(carray, index)           -> generate_deref carray index
    | Field(obj, fieldname)          -> generate_field obj fieldname
    | Invoc(recvr, fname, args)      -> generate_invocation recvr fname args
    | Unop(op, expr)                 -> stringify_unop op (expr_to_cstr expr) (fst expr)
    | Binop(lop, op, rop)            -> stringify_binop op (expr_to_cstr lop) (expr_to_cstr rop) ((fst lop), (fst rop))
    | Refine(args, ret, switch)      -> generate_refine args ret switch
    | Refinable(switch)              -> generate_refinable switch

and vdecl_to_cstr (vtype, vname) = vtype ^ " " ^ vname


let rec collect_dispatches_exprs exprs = List.iter collect_dispatches_expr exprs
and collect_dispatches_stmts stmts = List.iter collect_dispatches_stmt stmts
and collect_dispatches_expr (_, detail) = match detail with
    | This -> ()
    | Null -> ()
    | Id(_,_) -> ()
    | NewObj(_, _, args) -> collect_dispatches_exprs args
    | Literal(_) -> ()
    | Assign(mem, data) -> collect_dispatches_exprs [mem; data]
    | Deref(arr, idx) -> collect_dispatches_exprs [arr; idx]
    | Field(obj, _) -> collect_dispatches_expr obj
    | Invoc(recvr, _, args) -> collect_dispatches_exprs (recvr::args)
    | Unop(_, expr) -> collect_dispatches_expr expr
    | Binop(l, _, r) -> collect_dispatches_exprs [l; r]
    | Refine(args, ret, switch) -> collect_dispatch args ret switch
    | Refinable(switch) -> collect_dispatch_on switch
and collect_dispatches_stmt = function
    | Decl(_, Some(expr), _) -> collect_dispatches_expr expr
    | Decl(_, None, _) -> ()
    | If(iflist, env) -> collect_dispatches_clauses iflist
    | While(pred, body, _) -> collect_dispatches_expr pred; collect_dispatches_stmts body
    | Expr(expr, _) -> collect_dispatches_expr expr
    | Return(Some(expr), _) -> collect_dispatches_expr expr
    | Return(None, _) -> ()
and collect_dispatches_clauses pieces =
    let (preds, bodies) = List.split pieces in
    collect_dispatches_exprs (Util.filter_option preds);
    collect_dispatches_stmts (List.flatten bodies)
and collect_dispatch args ret = function
    | Sast.Switch(klass, cases, dispatch) -> dispatches := (klass, ret, (List.map fst args), dispatch, cases)::(!dispatches);
    | Sast.Test(_, _, _) -> raise(Failure("Impossible (wrong switch -- compiler error)"))
and collect_dispatch_on = function
    | Sast.Test(klass, klasses, dispatchby) -> dispatchon := (klass, klasses, dispatchby)::(!dispatchon);
    | Sast.Switch(_, _, _) -> raise(Failure("Impossible (wrong switch -- compiler error)"))
and collect_dispatch_func func = collect_dispatches_stmts func.body

(**  
    Takes an element from the dispatchon list and generates the test function for refinable.
    @param:  klasses - list of klasses in which the refinable method is defined for the method
             fuid - unique function name for the test function.
    @return: true or false
    Checks if the object on which refinable was invoked has an associated refinable method
    dispatched via this function that's being generated in one of the classes.
**)


let generate_testsw (klass, klasses, fuid) =
    let body = 
        match klasses with 
          [] -> "\treturn LIT_BOOL(0);"
        | _  -> 
                let predlist = List.map (fun kname -> "(this, "^kname^")") klasses in
                let ifpred  = String.concat " || " predlist in
                Format.sprintf "\tif ( %s )\n\t\treturn LIT_BOOL(1);\n\telse\n\t\treturn LIT_BOOL(0);\n" ifpred in
    Format.sprintf "t_Boolean %s (%s *this)\n{\n%s\n}\n\n" fuid klass body


(**
     Takes a dispatch element of the global dispatches list
     And generates the dispatch function - dispatcher which dispatches
     calls to refinable methods based on the RTTI of the this.
     @param ret - return type of the function
            args - arguments to the dispatcher and the dispatched method
            dispatch uid - unique function name for the dispatcher
            cases - list of classes and their corresponding uid of the invokable refinable methods.
**)

let generate_refinesw (klass, ret, args, dispatchuid, cases) =
    let rettype = match ret with
        | None -> "void "
        | Some(atype) -> Format.sprintf "%s *" atype in
    let this = (Format.sprintf "%s *" klass, "this") in
    let formals = List.mapi (fun i t -> (Format.sprintf "%s *" t, Format.sprintf "varg_%d" i)) args in
    let signature = String.concat ", " (List.map (fun (t, v) -> t ^ v) (this::formals)) in
    let actuals = List.map snd formals in
    let withthis kname = String.concat ", " ((Format.sprintf "(%s *) this" kname)::actuals) in
    let invoc fuid kname = Format.sprintf "%s(%s)" fuid (withthis kname) in
    let unroll_case (kname, fuid) =
        Format.sprintf "\tif( IS_CLASS( this, %s) )\n\t\t%s;\n" kname (invoc fuid kname) in
    let generated = List.map unroll_case cases in
    Format.sprintf "%s%s(%s)\n{\n%s\n}\n\n" rettype dispatchuid signature (String.concat "" generated)

(**
    Take a list of cast_stmts and return a body of c statements
    @param stmtlist A list of statements
    @return A body of c statements
*)
let rec cast_to_c_stmt indent cast =
    let indents = String.make indent '\t' in
    let stmts = cast_to_c_stmtlist (indent+1) in

    let cstmt = match cast with
        | Decl(vdecl, Some(expr), env) -> Format.sprintf "%s = (%s);" (vdecl_to_cstr vdecl) (expr_to_cstr expr)
        | Decl(vdecl, None, env) -> Format.sprintf "%s;" (vdecl_to_cstr vdecl)
        | If(iflist, env) -> cast_to_c_if_chain indent iflist
        | While(pred, [], env) -> Format.sprintf "while ( BOOL_OF( %s ) ) { }" (expr_to_cstr pred)
        | While(pred, body, env) -> Format.sprintf "while ( BOOL_OF( %s ) ) {\n%s\n%s}" (expr_to_cstr pred) (stmts body) indents
        | Expr(expr, env) -> Format.sprintf "( %s );" (expr_to_cstr expr)
        | Return(Some(expr), env) -> Format.sprintf "return ( %s );" (expr_to_cstr expr)
        | Return(_, env) -> "return;" in
    indents ^ cstmt

and cast_to_c_stmtlist indent stmts =
    String.concat "\n" (List.map (cast_to_c_stmt indent) stmts)

and cast_to_c_if_pred = function
    | None -> "else"
    | Some(ifpred) -> Format.sprintf "if ( BOOL_OF( %s ) )" (expr_to_cstr ifpred)

and cast_to_c_if_chain indent pieces =
    let indents = String.make indent '\t' in
    let stmts = cast_to_c_stmtlist (indent + 1) in
    let combine (pred, body) = Format.sprintf "%s {\n%s\n%s}" (cast_to_c_if_pred pred) (stmts body) indents in
    String.concat " else " (List.map combine pieces)


let cast_to_c_class_struct klass_name ancestors =
    let ancestor_var (vtype, vname) = Format.sprintf "%s *%s;" vtype vname in
    let ancestor_vars vars = String.concat "\n\t\t" (List.map ancestor_var vars) in
    let internal_struct (ancestor, vars) = match vars with
        | [] -> Format.sprintf "struct { BYTE empty_vars; } %s;" ancestor
        | _ -> Format.sprintf "struct {\n\t\t%s\n\t} %s;\n" (ancestor_vars vars) ancestor in
    let internals = String.concat "\n\n\t" (List.map internal_struct ancestors) in
    let meta = "\tClassInfo *meta;" in
    Format.sprintf "typedef struct {\n%s\n\n\t%s\n} %s;\n\n" meta internals klass_name

let cast_to_c_func cfunc =
    let ret_type = match cfunc.returns with
        | None -> "void "
        | Some(atype) -> Format.sprintf "%s *" atype in
    let body = match cfunc.body with
        | [] -> " { }"
        | body -> Format.sprintf "\n{\n%s\n}" (cast_to_c_stmtlist 1 body) in
    let params = (GenCast.get_tname cfunc.inklass, "this")::cfunc.formals in
    let signature = String.concat ", " (List.map (fun (t,v) -> t ^ " *" ^ v) params) in
    if cfunc.builtin then Format.sprintf "/* Place-holder for %s%s(%s) */" ret_type cfunc.name signature
    else Format.sprintf "\n%s%s(%s)%s\n" ret_type cfunc.name signature body

let cast_to_c_proto cfunc =
    let ret_type = match cfunc.returns with
        | None -> "void "
        | Some(atype) -> Format.sprintf "%s *" atype in
    let params = (GenCast.get_tname cfunc.inklass, "this")::cfunc.formals in
    let types = String.concat ", " (List.map (fun (t,v) -> t ^ " *") params) in
    let signature = Format.sprintf "%s%s(%s);" ret_type cfunc.name types in
    if cfunc.builtin then Format.sprintf "" else signature

let cast_to_c_proto_dispatch_on (klass, _, uid) =
    Format.sprintf "t_Boolean %s(%s *);" uid klass

let cast_to_c_proto_dispatch (klass, ret, args, uid, _) =
    let types = List.map (fun t -> t ^ " *") (klass::args) in
    let proto rtype = Format.sprintf "%s %s(%s);" rtype uid (String.concat ", " types) in
    match ret with
        | None -> proto "void"
        | Some(t) -> proto t

let cast_to_c_main mains =
    let main_fmt = ""^^"\tif (!strncmp(main, \"%s\", %d)) { %s(str_args); return 0; }" in
    let for_main (klass, uid) = Format.sprintf main_fmt klass (String.length klass + 1) uid in
    let switch = String.concat "\n" (List.map for_main mains) in
    Format.sprintf "int main(int argc, char **argv) {\n\tINIT_MAIN\n%s\n\tFAIL_MAIN\n\treturn 1;\n}" switch

let commalines input n =
    let newline string = String.length string >= n in
    let rec line_builder line rlines = function
        | [] -> List.rev (line::rlines)
        | str::rest ->
            let comma = match rest with [] -> false | _ -> true in
            let str = if comma then str ^ ", " else str in
            if newline line then line_builder str (line::rlines) rest
            else line_builder (line ^ str) rlines rest in
    match input with
        | [] -> []
        | [one] -> [one]
        | str::rest -> line_builder (str ^ ", ") [] rest

let print_class_strings = function
    | [] -> raise(Failure("Not even built in classes?"))
    | classes -> commalines (List.map (fun k -> "\"" ^ k ^ "\"") classes) 75

let print_class_enums = function
    | [] -> raise(Failure("Not even built in classes?"))
    | first::rest ->
        let first = first ^ " = 0" in
        commalines (List.map String.uppercase (first::rest)) 75

let setup_meta klass ancestors =
    let gen = List.length ancestors - 1 in
    let to_ptr klass = Format.sprintf "m_classes[%s]" (String.uppercase (GenCast.get_tname klass)) in
    let classes = List.map to_ptr ancestors in
    let ancestor_ptrs = List.map (fun f -> "\t\t" ^ f) (commalines classes 70) in
    Format.sprintf "ClassInfo M_%s = {\n\t.ancestors = {\n%s\n\t},\n\t.generation = %d,\n\t.class = m_classes[%s]\n};"
        (String.uppercase klass) (String.concat "\n" ancestor_ptrs) gen (String.uppercase (GenCast.get_tname klass))

let cast_to_c ((cdefs, funcs, mains, ancestry) : Cast.program) channel =
    let out string = Printf.fprintf channel "%s\n" string in
    let noblanks = function
        | "" -> ()
        | string -> Printf.fprintf channel "%s\n" string in
    let incl file = out (Format.sprintf "#include \"%s.h\"\n" file) in

    let comment string =
        let comments = Str.split (Str.regexp "\n") string in
        let commented = List.map (Format.sprintf " * %s") comments in
        out (Format.sprintf "\n\n/*\n%s\n */" (String.concat "\n" commented)) in

    let func_compare f g =
       let strcmp = Pervasives.compare f.name g.name in
       if f.builtin = g.builtin then strcmp else if f.builtin then -1 else 1 in
    let funcs = List.sort func_compare funcs in

    comment "Passing over code to find dispatch data.";
    List.iter collect_dispatch_func funcs;

    comment "Gamma preamble -- macros and such needed by various things";
    incl "gamma-preamble";

    comment "Ancestry meta-info to link to later.";
    let classes = List.map (fun (kls, _) -> GenCast.get_tname kls) (StringMap.bindings ancestry) in
    let class_strs = List.map (Format.sprintf "\t%s") (print_class_strings classes) in
    out (Format.sprintf "char *m_classes[] = {\n%s\n};" (String.concat "\n" class_strs));

    comment "Enums used to reference into ancestry meta-info strings.";
    let class_enums = List.map (Format.sprintf "\t%s") (print_class_enums classes) in
    out (Format.sprintf "enum m_class_idx {\n%s\n};" (String.concat "\n" class_enums));

    comment "Header file containing meta information for built in classes.";
    incl "gamma-builtin-meta";

    comment "Meta structures for each class.";
    let print_meta (klass, ancestors) =
        if StringSet.mem (GenCast.get_tname klass) GenCast.built_in_names then ()
        else out ((setup_meta klass ancestors) ^ "\n") in
    List.iter print_meta (StringMap.bindings ancestry);

    comment "Header file containing structure information for built in classes.";
    incl "gamma-builtin-struct";

    comment "Structures for each of the objects.";
    let print_class klass data =
        if StringSet.mem klass GenCast.built_in_names then ()
        else out (cast_to_c_class_struct klass data) in
    StringMap.iter print_class cdefs;

    comment "Header file containing information regarding built in functions.";
    incl "gamma-builtin-functions";

    comment "All of the function prototypes we need to do magic.";
    List.iter (fun func -> noblanks (cast_to_c_proto func)) funcs;

    comment "All the dispatching functions we need to continue the magic.";
    List.iter (fun d -> out (cast_to_c_proto_dispatch_on d)) (!dispatchon);
    List.iter (fun d -> out (cast_to_c_proto_dispatch d)) (!dispatches);

    comment "All of the functions we need to run the program.";
    List.iter (fun func -> out (cast_to_c_func func)) funcs;

    comment "Dispatch looks like this.";
    List.iter (fun d -> out (generate_testsw d)) (!dispatchon);
    List.iter (fun d -> out (generate_refinesw d)) (!dispatches);

    comment "The main.";
    out (cast_to_c_main mains);
