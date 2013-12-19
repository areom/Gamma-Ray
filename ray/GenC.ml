open Cast
open StringModules

let c_indent = "  "

let dispatches = ref []
let dispatchon = ref []

let lit_to_str lit = match lit with
    | Ast.Int(i) -> "LIT_INT("^(string_of_int i)^")"
    | Ast.Float(f) -> "LIT_FLOAT("^(string_of_float f)^")"
    | Ast.String(s) -> "LIT_STRING(\"" ^ s ^ "\")"  (* escapes were escaped during lexing *)
    | Ast.Bool(b) ->if b then "LIT_BOOL(1)" else "LIT_BOOL(0)"

let stringify_unop op rop rtype = match op with
    | Ast.Arithmetic(Ast.Neg) -> "NEG_"^rtype^rop
    | Ast.CombTest(Ast.Not)   -> "NEG_"^rtype^rop
    | _   -> raise (Failure "Unknown operator")

let stringify_arith op lop rop suffix = 
    let ops = suffix^"("^lop^" , "^rop^")" in
    match op with
    | Ast.Add  -> "ADD_"^ops
    | Ast.Sub  -> "SUB_"^ops
    | Ast.Prod -> "PROD_"^ops
    | Ast.Div  -> "DIV_"^ops
    | Ast.Mod  -> "MOD_"^ops
    | Ast.Neg  ->  raise(Failure "Unary operator")
    | Ast.Pow  -> Format.sprintf "pow(%s,%s)" lop rop

let stringify_numtest op lop rop = match op with
    | Ast.Eq   -> lop^" == "^rop
    | Ast.Neq  -> lop^" != "^rop
    | Ast.Less -> lop^" < "^rop
    | Ast.Grtr -> lop^" > "^rop
    | Ast.Leq  -> lop^" <= "^rop
    | Ast.Geq  -> lop^" >= "^rop

let stringify_combtest op lop rop = match op with
    | Ast.And  -> lop^" && "^rop
    | Ast.Or   -> lop^" || "^rop
    | Ast.Nand -> "!( "^lop^" && "^rop^" )"
    | Ast.Nor  -> "!( "^lop^" || "^rop^" )"
    | Ast.Xor  -> "!( "^lop^" == "^rop^" )"
    | Ast.Not  -> raise(Failure "Unary operator")

let stringify_binop op lop rop types = 
    let suffix = match types with
        | ("Integer", "Integer") -> "INT_INT"
        | ("Float", "Float")     -> "FLOAT_FLOAT"
        | ("Integer", "Float")   -> "INT_FLOAT"
        | ("Float", "Integer")   -> "FLOAT_INT" 
        | ("Boolean", "Boolean") -> "BOOL_BOOL" 
        | (_, _)                 -> raise(Failure "Binary operator")in
    match op with
    | Ast.Arithmetic(arith)  -> stringify_arith arith lop rop suffix
    | Ast.NumTest(numtest)   -> stringify_numtest numtest lop rop
    | Ast.CombTest(combtest) -> stringify_combtest combtest lop rop

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
        | Sast.Switch(cases, dispatch) ->
          dispatches := (ret, args, dispatch)::(!dispatches);
          let vals = List.map expr_to_cstr args in
          (match args with
              | [] -> Format.sprintf "%s(this)" dispatch
              | _ -> Format.sprintf "%s(this, %s)" dispatch (String.concat ", " vals))
        | _ -> raise(Failure("Wrong switch applied to refine -- compiler error.")) in
    let generate_refinable = function
        | Sast.Test(klasses, dispatchby) ->
          dispatchon := (klasses, dispatchby)::(!dispatchon);
          Format.sprintf "%s(this)" dispatchby
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
        | While(pred, [], env) -> Format.sprintf "while ( %s ) { }" (expr_to_cstr pred)
        | While(pred, body, env) -> Format.sprintf "while ( %s ) {\n%s\n%s}" (expr_to_cstr pred) (stmts body) indents
        | Expr(expr, env) -> Format.sprintf "( %s );" (expr_to_cstr expr)
        | Return(Some(expr), env) -> Format.sprintf "return ( %s );" (expr_to_cstr expr)
        | Return(_, env) -> "return;" in
    indents ^ cstmt

and cast_to_c_stmtlist indent stmts =
    String.concat "\n" (List.map (cast_to_c_stmt indent) stmts)

and cast_to_c_if_pred = function
    | None -> "else"
    | Some(ifpred) -> Format.sprintf "if ( %s )" (expr_to_cstr ifpred)

and cast_to_c_if_chain indent pieces =
    let indents = String.make indent '\t' in
    let stmts = cast_to_c_stmtlist (indent + 1) in
    let combine (pred, body) = Format.sprintf "%s {\n%s%s}" (cast_to_c_if_pred pred) (stmts body) indents in
    String.concat " else " (List.map combine pieces)


let cast_to_c_class_struct klass_name ancestors =
    let ancestor_var (vtype, vname) = Format.sprintf "%s %s;" vtype vname in
    let ancestor_vars vars = String.concat "\n\t\t" (List.map ancestor_var vars) in
    let internal_struct (ancestor, vars) = match vars with
        | [] -> Format.sprintf "struct { BYTE empty_vars; } %s;" ancestor
        | _ -> Format.sprintf "struct {\n\t\t%s\n\t} %s;\n" (ancestor_vars vars) ancestor in
    let internals = String.concat "\n\n\t" (List.map internal_struct ancestors) in
    let metain = String.concat "\n\t\t" ["char **ancestor;"; "int generation;"; "char *class;"] in
    let meta = Format.sprintf "\tstruct {\n\t\t%s\n\t} meta;" metain in
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
    if cfunc.builtin
        then Format.sprintf "/* Place-holder for %s%s(%s) */\n\n" ret_type cfunc.name signature
        else Format.sprintf "%s%s(%s)%s\n\n" ret_type cfunc.name signature body

let cast_to_c_main mains =
    let main_fmt = ""^^"\tif (!strncmp(main, \"%s\", %d)) { %s(str_args); return 0;}" in
    let for_main (klass, uid) = Format.sprintf main_fmt klass (String.length klass + 1) uid in
    let switch = String.concat "\n" (List.map for_main mains) in
    Format.sprintf "int main(int argc, char **argv) {\n\tINIT_MAIN\n%s\n\tFAIL_MAIN\n\treturn 1;\n}" switch

let cast_to_c ((cdefs, funcs, mains) : Cast.program) channel =
    let out string = Printf.fprintf channel "%s\n" string in
    let comment string =
        let comments = Str.split (Str.regexp "\n") string in
        let commented = List.map (Format.sprintf " * %s") comments in
        out (Format.sprintf "\n\n/*\n%s\n */" (String.concat "\n" commented)) in

    comment "Structures for each of the objects.";
    let print_class klass data =
        if StringSet.mem klass GenCast.built_in_names then ()
        else out (cast_to_c_class_struct klass data) in
    StringMap.iter print_class cdefs;

    comment "All of the functions we need to run the program.";
    List.iter (fun func -> out (cast_to_c_func func)) funcs;

    comment "The main.";
    out (cast_to_c_main mains);
