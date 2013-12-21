open Ast
open Sast
open Klass
open StringModules
open Util
open GlobalData

(** Module to take an AST and build the sAST out of it. *)

(**
    Update an environment to have a variable
    @param mode The mode the variable is in (instance, local)
    @param vtype The type of the variable
    @param vname The name of the variable
    @return A function that will update an environment passed to it.
  *)
let env_update mode (vtype, vname) env = match map_lookup vname env, mode with
    | None, _ -> StringMap.add vname (vtype, mode) env
    | Some((otype, Local)), Local -> raise(Failure("Local variable " ^ vname ^ " loaded twice, once with type " ^ otype ^ " and then with type " ^ vtype ^ "."))
    | _, Local -> StringMap.add vname (vtype, mode) env
    | _, _ -> raise(Failure("Instance variable declared twice in ancestry chain -- this should have been detected earlier; compiler error."))
let env_updates mode = List.fold_left (fun env vdef -> env_update mode vdef env)
let add_ivars klass env level =
    let sects = match level with
        | Publics -> [Publics]
        | Protects -> [Publics; Protects]
        | Privates -> [Publics; Protects; Privates]
        | _ -> raise(Failure("Inappropriate class section - access level."))  in
    let filter (s, _) = List.mem s sects in
    let vars = Klass.klass_to_variables klass in
    let eligible = List.flatten (List.map snd (List.filter filter vars)) in
    env_updates (Instance(klass.klass)) env eligible

(** Marker for being in the current class -- ADT next time *)
let current_class = "_CurrentClassMarker_"

(** Marker for the null type -- ADT next time *)
let null_class = "_Null_"

(** Return whether an expression is a valid lvalue or not *)
let is_lvalue (expr : Ast.expr) = match expr with
    | Ast.Id(_) -> true
    | Ast.Field(_, _) -> true
    | Ast.Deref(_, _) -> true
    | _ -> false

(**
    Map a literal value to its type
    @param litparam a literal
    @return A string representing the type.
  *)
let getLiteralType litparam = match litparam with
    | Ast.Int(i) -> "Integer"
    | Ast.Float(f) -> "Float"
    | Ast.String(s) -> "String"
    | Ast.Bool(b) -> "Boolean"

(**
    Map a return type string option to a return type string
    @param ret_type The return type.
    @return The return type -- Void or its listed type.
  *)
let getRetType ret_type = match ret_type with
    | Some(retval) -> retval
    | None -> "Void"

(**
    Given a class_data record, a class name, an environment, and an Ast.expr expression,
    return a Sast.expr expression.
    @param klass_data A class_data record
    @param kname The name of of the current class
    @param env The local environment (instance and local variables so far declared)
    @param exp An expression to eval to a Sast.expr value
    @return A Sast.expr expression, failing when there are issues.
  *)
let rec eval klass_data kname mname isstatic env exp =
    let eval' expr = eval klass_data kname mname isstatic env expr in
    let eval_exprlist elist = List.map eval' elist in

    let get_field expr mbr =
        let (recvr_type, _) as recvr = eval' expr in
        let this = (recvr_type = current_class) in
        let recvr_type = if this then kname else recvr_type in
        let field_type = match Klass.class_field_far_lookup klass_data recvr_type mbr this with
            | Left((_, vtyp, _)) -> vtyp
            | Right(true) -> raise(Failure("Field " ^ mbr ^ " is not accessible in " ^ recvr_type ^ " from " ^ kname ^ "."))
            | Right(false) -> raise(Failure("Unknown field " ^ mbr ^ " in the ancestry of " ^ recvr_type ^ ".")) in
        (field_type, Sast.Field(recvr, mbr)) in

    let get_invoc expr methd elist =
        let (recvr_type, _) as recvr = eval' expr in
        let arglist = eval_exprlist elist in
        let this = (recvr_type = current_class) in
        let _ = if (this && isstatic)
            then raise(Failure(Format.sprintf "Cannot invoke %s on %s in %s for %s is static." methd mname kname mname))
            else () in
        let recvr_type = if this then kname else recvr_type in
        let argtypes = List.map fst arglist in
        let mfdef = match Klass.best_inherited_method klass_data recvr_type methd argtypes this with
            | None when this -> raise(Failure(Format.sprintf "Method %s not found ancestrally in %s (this=%b)" methd recvr_type this))
            | None -> raise(Failure("Method " ^ methd ^ " not found (publically) in the ancestry of " ^ recvr_type ^ "."))
            | Some(fdef) -> fdef in
        let mfid = if mfdef.builtin then BuiltIn mfdef.uid else FuncId mfdef.uid in
        (getRetType mfdef.returns, Sast.Invoc(recvr, methd, arglist, mfid)) in

    let get_init class_name exprlist =
        let arglist = eval_exprlist exprlist in
        let argtypes = List.map fst arglist in
        let mfdef = match best_method klass_data class_name "init" argtypes [Ast.Publics] with
            | None       -> raise(Failure "Constructor not found")
            | Some(fdef) -> fdef in
        let mfid = if mfdef.builtin then BuiltIn mfdef.uid else FuncId mfdef.uid in
        (class_name, Sast.NewObj(class_name, arglist, mfid)) in

    let get_assign e1 e2 =
        let (t1, t2) = (eval' e1, eval' e2) in
        let (type1, type2) = (fst t1, fst t2) in
        match is_subtype klass_data type2 type1, is_lvalue e1 with
            | _, false -> raise(Failure "Assigning to non-lvalue")
            | false, _ -> raise(Failure "Assigning to incompatible types")
            | _ -> (type1, Sast.Assign(t1, t2)) in

    let get_binop e1 op e2 =
        let isCompatible typ1 typ2 =
            if is_subtype klass_data typ1 typ2 then typ2
            else if is_subtype klass_data typ2 typ1 then typ1
            else raise (Failure (Format.sprintf "Binop takes incompatible types: %s %s" typ1 typ2)) in
        let (t1, t2) = (eval' e1, eval' e2) in
        let gettype op (typ1,_) (typ2,_) = match op with
            | Ast.Arithmetic(Neg) -> raise(Failure("Negation is not a binary operation!"))
            | Ast.CombTest(Not) -> raise(Failure("Boolean negation is not a binary operation!"))
            | Ast.Arithmetic(_) -> isCompatible typ1 typ2
            | Ast.NumTest(_)
            | Ast.CombTest(_) -> ignore(isCompatible typ1 typ2); "Boolean" in
        (gettype op t1 t2, Sast.Binop(t1,op,t2)) in

    let get_refine rname elist desired =
        let arglist = eval_exprlist elist in
        let argtypes = List.map fst arglist in
        let refines = Klass.refine_on klass_data kname mname rname argtypes desired in
        let switch = List.map (fun (f : Ast.func_def) -> (f.inklass, f.uid)) refines in
        (getRetType desired, Sast.Refine(rname, arglist, desired, Switch(kname, switch, UID.uid_counter ()))) in

    let get_refinable rname =
        let refines = Klass.refinable_lookup klass_data kname mname rname in
        let klasses = List.map (fun (f : Ast.func_def) -> f.inklass) refines in
        ("Boolean", Sast.Refinable(rname, Test(kname, klasses, UID.uid_counter ()))) in

    let get_deref e1 e2 =
        let expectArray typename = match Str.last_chars typename 2 with
            | "[]" -> Str.first_chars typename (String.length typename - 2)
            | _  -> raise (Failure "Not an array type") in
        let (t1, t2) = (eval' e1, eval' e2) in
        let getArrayType (typ1, _) (typ2, _) = match typ2 with
            | "Integer" -> expectArray typ1
            | _ -> raise(Failure "Dereferencing invalid") in
        (getArrayType t1 t2, Sast.Deref(t1, t2)) in

    let get_unop op expr = match op with
        | Ast.Arithmetic(Neg) -> let (typ, _) as evaled = eval' expr in (typ, Sast.Unop(op, evaled))
        | Ast.CombTest(Not) -> ("Boolean", Sast.Unop(op, eval' expr))
        | _ -> raise(Failure("Unknown binary operator " ^ Inspector.inspect_ast_op op ^ " given.")) in

    let lookup_type id = match map_lookup id env with
        | None -> raise(Failure("Unknown id " ^ id ^ " in environment built around " ^ kname ^ ", " ^ mname ^ "."))
        | Some((vtype, _)) -> vtype in

    match exp with
        | Ast.This -> (current_class, Sast.This)
        | Ast.Null -> (null_class, Sast.Null)
        | Ast.Id(vname) -> (lookup_type vname, Sast.Id(vname))
        | Ast.Literal(lit) -> (getLiteralType lit, Sast.Literal(lit))
        | Ast.NewObj(s1, elist) -> get_init s1 elist
        | Ast.Field(expr, mbr) -> get_field expr mbr
        | Ast.Invoc(expr, methd, elist) -> get_invoc expr methd elist
        | Ast.Assign(e1, e2) -> get_assign e1 e2
        | Ast.Binop(e1,op,e2) -> get_binop e1 op e2
        | Ast.Refine(s1, elist, soption) -> get_refine s1 elist soption
        | Ast.Deref(e1, e2) -> get_deref e1 e2
        | Ast.Refinable(s1) -> get_refinable s1
        | Ast.Unop(op, expr) -> get_unop op expr
        | Ast.Anonymous(atype, args, body) -> (atype, Sast.Anonymous(atype, args, body)) (* Delay evaluation *)

(**
    Given a class_data record, the name of the current class, a list of AST statements,
    and an initial environment, enumerate the statements and attach the environment at
    each step to that statement, yielding Sast statements. Note that when there is an
    issue the function will raise Failure.
    @param klass_data A class_data record
    @param kname The name of the class that is the current context.
    @param stmts A list of Ast statements
    @param initial_env An initial environment
    @return A list of Sast statements
  *)
let rec attach_bindings klass_data kname mname isvoid isstatic stmts initial_env =
    (* Calls that go easy on the eyes *)
    let eval' = eval klass_data kname mname isstatic in
    let attach' = attach_bindings klass_data kname mname isvoid isstatic in
    let eval_exprlist env elist = List.map (eval' env) elist in

    let rec get_superinit kname arglist =
        let parent = StringMap.find kname klass_data.parents in
        let argtypes = List.map fst arglist in
        match best_method klass_data parent "init" argtypes [Ast.Publics; Ast.Protects] with
            | None       -> raise(Failure "Cannot find super init")
            | Some(fdef) -> fdef.uid in

    (* Helper function for building a predicate expression *)
    let build_predicate pred_env exp = match eval' pred_env exp with
        | ("Boolean", _) as evaled -> evaled
        | _ -> raise (Failure "Predicates must be boolean") in

    (* Helper function for building an optional expression *)
    let opt_eval opt_expr opt_env = match opt_expr with
        | None -> None
        | Some(exp) -> Some(eval' opt_env exp) in

    (* For each kind of statement, build the associated Sast statment *)
    let build_ifstmt iflist if_env =
        let build_block if_env (exp, slist) =
            let exprtyp = match exp with
                | None -> None
                | Some exp -> Some(build_predicate if_env exp) in
            (exprtyp, attach' slist if_env) in
        Sast.If(List.map (build_block if_env) iflist, if_env) in

    let build_whilestmt expr slist while_env =
        let exprtyp = build_predicate while_env expr in
        let stmts = attach' slist while_env in
        Sast.While(exprtyp, stmts, while_env) in

    let build_declstmt ((vtype, vname) as vdef) opt_expr decl_env =
        if Klass.is_type klass_data vtype then Sast.Decl(vdef, opt_eval opt_expr decl_env, decl_env)
        else raise(Failure("Variable " ^ vname ^ " in the method " ^ mname ^ " in the class " ^ kname ^ " has unknown type " ^ vtype ^ ".")) in
    let build_returnstmt opt_expr ret_env = match opt_expr, isvoid with
        | None, false -> raise(Failure("Void return from non-void function " ^ mname ^ " in klass " ^ kname ^ "."))
        | Some(_), true -> raise(Failure("Non-void return from void function " ^ mname ^ " in klass " ^ kname ^ "."))
        | _, _ -> Sast.Return(opt_eval opt_expr ret_env, ret_env) in
    let build_exprstmt expr expr_env = Sast.Expr(eval' expr_env expr, expr_env) in
    let build_superstmt expr_list super_env =
        let arglist = eval_exprlist super_env expr_list in
        Sast.Super(arglist, get_superinit kname arglist, super_env)
    in
    (* Ast statement -> (Sast.Statement, Environment Update Option) *)
    let updater in_env = function
        | Ast.While(expr, slist)   -> (build_whilestmt expr slist in_env, None)
        | Ast.If(iflist)           -> (build_ifstmt iflist in_env, None)
        | Ast.Decl(vdef, opt_expr) -> (build_declstmt vdef opt_expr in_env, Some(vdef))
        | Ast.Expr(expr)           -> (build_exprstmt expr in_env, None)
        | Ast.Return(opt_expr)     -> (build_returnstmt opt_expr in_env, None)
        | Ast.Super(exprs)         -> (build_superstmt exprs in_env, None) in

    (* Function to fold a statement into a growing reverse list of Sast statements *)
    let build_env (output, acc_env) stmt =
        let (node, update) = updater acc_env stmt in
        let updated_env = match update with
            | None -> acc_env
            | Some(vdef) -> env_update Local vdef acc_env in
        (node::output, updated_env) in

    List.rev (fst(List.fold_left build_env ([], initial_env) stmts))

(**
    Given a list of statements, return whether every execution path therein returns
    @param stmts A bunch of Ast.stmts
    @return true or false based on whether everything returns a value.
  *)
let rec does_return_stmts (stmts : Ast.stmt list) = match stmts with
    | [] -> false
    | Return(None)::_ -> false
    | Return(_)::_ -> true
    | If(pieces)::rest -> does_return_clauses pieces || does_return_stmts rest
    | _::rest -> does_return_stmts rest
(**
    Given a collection of if clauses, return whether they represent a return from the function.
    @param pieces If clauses (option expr, stmt list)
    @return whether or not it can be determined that a return is guaranteed here.
  *)
and does_return_clauses pieces =
    let (preds, bodies) = List.split pieces in
    List.mem None preds && List.for_all does_return_stmts bodies

(**
    Given a class_data record, an Ast.func_def, an an initial environment,
    convert the func_def to a Sast.func_def. Can raise failure when there
    are issues with the statements / expressions in the function.
    @param klass_data A class_data record
    @param func An Ast.func_def to transform
    @param initial_env The initial environment
    @return A Sast.func_def value
  *)
let ast_func_to_sast_func klass_data (func : Ast.func_def) initial_env =
    let with_params = List.fold_left (fun env vdef -> env_update Local vdef env) initial_env func.formals in
    let isvoid = match func.returns with None -> true | _ -> false in
    let sast_func : Sast.func_def =
        {   returns = func.returns;
            host = func.host;
            name = func.name;
            formals = func.formals;
            static = func.static;
            body = attach_bindings klass_data func.inklass func.name isvoid func.static func.body with_params;
            section = func.section;
            inklass = func.inklass;
            uid = func.uid;
            builtin = func.builtin } in
    if not func.builtin && not isvoid && not (does_return_stmts func.body)
        then raise(Failure(Format.sprintf "The function %s in %s does not return on all execution paths" (full_signature_string func) func.inklass))
        else sast_func

(**
    Given a class_data record, an Ast.member_def, and an initial environment,
    convert the member into an Sast.member_def. May raise failure when there
    are issues in the statements / expressions in the member.
    @param klass_data A class_data record.
    @param mem An Ast.member_def value
    @param initial_env An environment of variables
    @return A Sast.member_def
  *)
let ast_mem_to_sast_mem klass_data (mem : Ast.member_def) initial_env =
    let change func = ast_func_to_sast_func klass_data func initial_env in
    let transformed : Sast.member_def = match mem with
        | Ast.VarMem(v) -> Sast.VarMem(v)
        | Ast.MethodMem(m) -> Sast.MethodMem(change m)
        | Ast.InitMem(m) -> Sast.InitMem(change m) in
    transformed

let init_calls_super (aklass : Sast.class_def) =
    let validate_init func_def = match func_def.builtin, func_def.body with
        | true, _ -> true
        | _, (Super(_,_,_)::_) -> true
        | _, _ -> false in
    let grab_init = function
        | InitMem(m) -> Some(m)
        | _ -> None in
    let get_inits mems = Util.filter_option (List.map grab_init mems) in
    let s = aklass.sections in
    let inits = List.flatten (List.map get_inits [s.publics; s.protects; s.privates]) in
    List.for_all validate_init inits

(**
    Given a class_data object and an Ast.class_def, return a Sast.class_def
    object. May fail when there are issues in the statements / expressions.
    @param klass_data A class_data record value
    @param ast_klass A class to transform
    @return The transformed class.
  *)
let ast_to_sast_klass klass_data (ast_klass : Ast.class_def) =
    let s : Ast.class_sections_def = ast_klass.sections in
    let rec update_env env sect (klass : Ast.class_def) =
        let env = add_ivars klass env sect in
        match klass.klass with
            | "Object" -> env
            | _ -> let parent = Klass.klass_to_parent klass in
                   let pclass = StringMap.find parent klass_data.classes in
                   update_env env Protects pclass in
    let env = update_env StringMap.empty Privates ast_klass in

    let mems = List.map (fun m -> ast_mem_to_sast_mem klass_data m env) in
    let funs = List.map (fun f -> ast_func_to_sast_func klass_data f env) in

    let sections : Sast.class_sections_def =
        {   publics = mems s.publics;
            protects = mems s.protects;
            privates = mems s.privates;
            refines = funs s.refines;
            mains = funs s.mains } in

    let sast_klass : Sast.class_def =
        {   klass = ast_klass.klass;
            parent = ast_klass.parent;
            sections = sections } in

    if init_calls_super sast_klass then sast_klass
    else raise(Failure(Format.sprintf "%s's inits don't always call super as their first statement (maybe empty body, maybe something else)." sast_klass.klass))

(**
    @param ast An ast program
    @return A sast program
*)
let ast_to_sast klass_data =
    let klasses = StringMap.bindings klass_data.classes in
    let to_sast (_, klass) = ast_to_sast_klass klass_data klass in
    List.map to_sast klasses
