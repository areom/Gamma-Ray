open Cast
open StringModules

let c_indent = "  "

let dispatches = ref []
let dispatchon = ref []

let lit_to_str lit = match lit with
    | Ast.Int(i) -> string_of_int i
    | Ast.Float(f) -> string_of_float f
    | Ast.String(s) -> "\"" ^ s ^ "\""  (* escapes were escaped during lexing *)
    | Ast.Bool(b) ->if b then "1" else "0"

let stringify_unop op rop = match op with
    | Ast.Arithmetic(Ast.Neg) -> "-"^rop
    | Ast.CombTest(Ast.Not)   -> "!"^rop
    | _   -> raise (Failure "Unknown operator")

let stringify_arith op lop rop = match op with
    | Ast.Add  -> lop^" + "^rop
    | Ast.Sub  -> lop^" - "^rop
    | Ast.Prod -> lop^" * "^rop
    | Ast.Div  -> lop^" / "^rop
    | Ast.Mod  -> lop^" % "^rop
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

let stringify_binop op lop rop = match op with
    | Ast.Arithmetic(arith)  -> stringify_arith arith lop rop
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
    | Unop(op, expr)                 -> stringify_unop op (expr_to_cstr expr)
    | Binop(lop, op, rop)            -> stringify_binop op (expr_to_cstr lop) (expr_to_cstr rop)
    | Refine(args, ret, switch)      -> generate_refine args ret switch
    | Refinable(switch)              -> generate_refinable switch

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
    let c_stmt = match caststmt with
        | Decl(vdecl, Some(expr), env) -> Format.sprintf "%s = (%s);\n" (vdecl_to_cstr vdecl) (expr_to_cstr expr)
        | Decl(vdecl, None, env) -> Format.sprintf "%s;\n" (vdecl_to_cstr vdecl)
        | If(iflist, env) -> stringify_list (List.mapi ifstmt_to_str iflist)
        | While(pred, body, env) -> Format.sprintf "while ( %s ) {\n %s \n}\n" (expr_to_cstr pred) (cast_to_cstmtlist body)
        | Expr(expr, env) -> Format.sprintf "( %s );\n" (expr_to_cstr expr)
        | Return(Some(expr), env) -> Format.sprintf "return ( %s );\n" (expr_to_cstr expr)
        | Return(_, env) -> "return;\n" in
    c_indent ^ c_stmt

let cast_to_c_class_struct klass_name ancestors =
    let ancestor_var (vtype, vname) = Format.sprintf "%s %s;" vtype vname in
    let ancestor_vars vars = String.concat "\n\t\t" (List.map ancestor_var vars) in
    let internal_struct (ancestor, vars) = match vars with
        | [] -> Format.sprintf "struct { /* empty */ } %s;" ancestor
        | _ -> Format.sprintf "struct {\n\t\t%s\n\t} %s\n" (ancestor_vars vars) ancestor in
    let internals = String.concat "\n\n\t" (List.map internal_struct ancestors) in
    let meta = Format.sprintf "\tstruct { char **ancestors; } meta;" in
    Format.sprintf "\n\ntypedef struct {\n%s\n\n\t%s\n} %s;" meta internals klass_name

let cast_to_c_func cfunc =
    let ret_type = match cfunc.returns with
        | None -> "void"
        | _ -> GenCast.get_tname "Object" in
    let stmts = cast_to_cstmtlist cfunc.body in
    let args = "this" :: (List.map snd cfunc.formals) in
    let params = (GenCast.get_tname cfunc.inklass, "this")::cfunc.formals in
    let body = match cfunc.builtin, cfunc.returns with
        | None, _ -> stmts
        | Some(builtin), None -> Format.sprintf "%s(%s);" builtin (String.concat ", " args)
        | Some(builtin), _ -> Format.sprintf "return %s(%s);" builtin (String.concat ", " args) in
    let signature = String.concat ", " (List.map (fun (t,v) -> t ^ " " ^ v) params) in
    Format.sprintf "%s %s(%s)\n{\n%s\n}" ret_type cfunc.name signature body

let cast_to_c_main mains =
    let main_fmt = ""^^"if (!strncmp(main, \"%s\", %d)) { %s(str_args); return 0;}" in
    let for_main (klass, uid) = Format.sprintf main_fmt klass (String.length klass + 1) uid in
    let switch = String.concat "\n" (List.map for_main mains) in
    Format.sprintf "int main(int argc, char **argv) {\n\tINIT_MAIN\n%s\n\tFAIL_MAIN\n\treturn 0;\n}" switch

let cast_to_c ((cdefs, funcs, mains) : Cast.program) channel =
    let out string = Printf.fprintf channel "%s\n" string in
    let comment string = out (Format.sprintf "\n\n/*\n%s\n*/" string) in

    comment "Structures for each of the objects.";
    StringMap.iter (fun klass data -> out (cast_to_c_class_struct klass data)) cdefs;

    comment "All of the functions we need to run the program.";
    List.iter (fun func -> out (cast_to_c_func func)) funcs;

    comment "The main.";
    out (cast_to_c_main mains);
