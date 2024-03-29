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

(** Empty environment *)
let empty_environment = StringMap.empty

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
    Update a refinement switch based on updated data.
  *)
let rec update_refinements_stmts klass_data kname mname = List.map (update_refinements_stmt klass_data kname mname)
and update_refinements_exprs klass_data kname mname = List.map (update_refinements_expr klass_data kname mname)
and update_refinements_expr klass_data kname mname (atype, expr) =
    let doexp = update_refinements_expr klass_data kname mname in
    let doexps = update_refinements_exprs klass_data kname mname in

    let get_refine rname arglist desired uid =
        let argtypes = List.map fst arglist in
        let refines = Klass.refine_on klass_data kname mname rname argtypes desired in
        let switch = List.map (fun (f : Ast.func_def) -> (f.inklass, f.uid)) refines in
        (getRetType desired, Sast.Refine(rname, arglist, desired, Switch(kname, switch, uid))) in

    let get_refinable rname uid =
        let refines = Klass.refinable_lookup klass_data kname mname rname in
        let klasses = List.map (fun (f : Ast.func_def) -> f.inklass) refines in
        ("Boolean", Sast.Refinable(rname, Test(kname, klasses, uid))) in

    match expr with
        | Sast.Refine(rname, args, desired, Switch(_, _, uid)) -> get_refine rname args desired uid
        | Sast.Refine(_, _, _, _) -> raise(Failure("Test in switch."))
        | Sast.Refinable(rname, Test(_, _, uid)) -> get_refinable rname uid
        | Sast.Refinable(_, _) -> raise(Failure("Switch in test."))

        | Sast.Anonymous(_, _, _) -> raise(Failure("Anonymous detected during reswitching."))

        | Sast.This -> (atype, Sast.This)
        | Sast.Null -> (atype, Sast.Null)
        | Sast.Id(id) -> (atype, Sast.Id(id))
        | Sast.NewObj(klass, args, uid) -> (atype, Sast.NewObj(klass, doexps args, uid))
        | Sast.Literal(lit) -> (atype, Sast.Literal(lit))
        | Sast.Assign(l, r) -> (atype, Sast.Assign(doexp l, doexp r))
        | Sast.Deref(l, r) -> (atype, Sast.Deref(doexp l, doexp r))
        | Sast.Field(e, m) -> (atype, Sast.Field(doexp e, m))
        | Sast.Invoc(r, m, args, uid) -> (atype, Sast.Invoc(doexp r, m, doexps args, uid))
        | Sast.Unop(op, e) -> (atype, Sast.Unop(op, doexp e))
        | Sast.Binop(l, op, r) -> (atype, Sast.Binop(doexp l, op, doexp r))
and update_refinements_stmt klass_data kname mname stmt =
    let doexp = update_refinements_expr klass_data kname mname in
    let doexps = update_refinements_exprs klass_data kname mname in
    let dostmts = update_refinements_stmts klass_data kname mname in
    let docls = update_refinements_clauses klass_data kname mname in

    match stmt with
        | Sast.Decl(_, None, _) as d -> d
        | Sast.Decl(vdef, Some(e), env) -> Sast.Decl(vdef, Some(doexp e), env)
        | Sast.If(pieces, env) -> Sast.If(docls pieces, env)
        | Sast.While(pred, body, env) -> Sast.While(doexp pred, dostmts body, env)
        | Sast.Expr(expr, env) -> Sast.Expr(doexp expr, env)
        | Sast.Return(None, _) as r -> r
        | Sast.Return(Some(e), env) -> Sast.Return(Some(doexp e), env)
        | Sast.Super(args, uid, super, env) -> Sast.Super(doexps args, uid, super, env)
and update_refinements_clauses (klass_data : class_data) (kname : string) (mname : string) (pieces : (Sast.expr option * Sast.sstmt list) list) : (Sast.expr option * Sast.sstmt list) list =
    let dobody = update_refinements_stmts klass_data kname mname in
    let dopred = update_refinements_expr klass_data kname mname in

    let mapping = function
        | (None, body) -> (None, dobody body)
        | (Some(e), body) -> (Some(dopred e), dobody body) in
    List.map mapping pieces

let update_refinements_func klass_data (func : Sast.func_def) =
    { func with body = update_refinements_stmts klass_data func.inklass func.name func.body }

let update_refinements_member klass_data = function
    | Sast.InitMem(i) -> Sast.InitMem(update_refinements_func klass_data i)
    | Sast.MethodMem(m) -> Sast.MethodMem(update_refinements_func klass_data m)
    | v -> v

let update_refinements_klass klass_data (klass : Sast.class_def) =
    let mems = List.map (update_refinements_member klass_data) in
    let funs = List.map (update_refinements_func klass_data) in
    let s = klass.sections in
    let sects = 
        { publics = mems s.publics;
          protects = mems s.protects;
          privates = mems s.privates;
          mains = funs s.mains;
          refines = funs s.refines } in
    { klass with sections = sects }

let update_refinements klass_data (klasses : Sast.class_def list) =
    List.map (update_refinements_klass klass_data) klasses

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

    let cast_to klass (_, v) = (klass, v) in

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
        (getRetType mfdef.returns, Sast.Invoc(cast_to (mfdef.inklass) recvr, methd, arglist, mfid)) in

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

    let get_new_arr atype args =
        let arglist = eval_exprlist args in
        if List.exists (fun (t, _) -> t <> "Integer") arglist
           then raise(Failure "Size of an array dimensions does not correspond to an integer.")
           else (atype, Sast.NewObj(atype, arglist, ArrayAlloc(UID.uid_counter ()))) in

    let get_new_obj atype args = try
        let index = String.index atype '[' in
        let dimensions = (String.length atype - index) / 2 in
        match List.length args with
            | n when n > dimensions -> raise(Failure("Cannot allocate array, too many dimensions given."))
            | n when n < dimensions -> raise(Failure("Cannot allocate array, too few dimensions given."))
            | 0 -> (null_class, Sast.Null)
            | _ -> get_new_arr atype args
        with Not_found -> get_init atype args in

    match exp with
        | Ast.This -> (current_class, Sast.This)
        | Ast.Null -> (null_class, Sast.Null)
        | Ast.Id(vname) -> (lookup_type vname, Sast.Id(vname))
        | Ast.Literal(lit) -> (getLiteralType lit, Sast.Literal(lit))
        | Ast.NewObj(s1, elist) -> get_new_obj s1 elist
        | Ast.Field(expr, mbr) -> get_field expr mbr
        | Ast.Invoc(expr, methd, elist) -> get_invoc expr methd elist
        | Ast.Assign(e1, e2) -> get_assign e1 e2
        | Ast.Binop(e1,op,e2) -> get_binop e1 op e2
        | Ast.Refine(s1, elist, soption) -> get_refine s1 elist soption
        | Ast.Deref(e1, e2) -> get_deref e1 e2
        | Ast.Refinable(s1) -> get_refinable s1
        | Ast.Unop(op, expr) -> get_unop op expr
        | Ast.Anonymous(atype, args, body) -> (atype, Sast.Anonymous(atype, eval_exprlist args, body)) (* Delay evaluation *)

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
let rec attach_bindings klass_data kname mname meth_ret isstatic stmts initial_env =
    (* Calls that go easy on the eyes *)
    let eval' = eval klass_data kname mname isstatic in
    let attach' = attach_bindings klass_data kname mname meth_ret isstatic in
    let eval_exprlist env elist = List.map (eval' env) elist in

    let rec get_superinit kname arglist =
        let parent = StringMap.find kname klass_data.parents in
        let argtypes = List.map fst arglist in
        match best_method klass_data parent "init" argtypes [Ast.Publics; Ast.Protects] with
            | None       -> raise(Failure "Cannot find super init")
            | Some(fdef) -> fdef in

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
        if not (Klass.is_type klass_data vtype) then raise(Failure(Format.sprintf "%s in %s.%s has unknown type %s." vname kname mname vtype))
        else match opt_eval opt_expr decl_env with
            | Some((atype, _)) as evaled -> if not (Klass.is_subtype klass_data atype vtype)
                then raise(Failure(Format.sprintf "%s in %s.%s is type %s but is assigned a value of type %s." vname kname mname vtype atype))
                else Sast.Decl(vdef, evaled, decl_env)
            | None -> Sast.Decl(vdef, None, decl_env) in

    let check_ret_type ret_type = match ret_type, meth_ret with
        | None, Some(_) -> raise(Failure("Void return from non-void function " ^ mname ^ " in klass " ^ kname ^ "."))
        | Some(_), None -> raise(Failure("Non-void return from void function " ^ mname ^ " in klass " ^ kname ^ "."))
        | Some(r), Some(t) -> if not (Klass.is_subtype klass_data r t) then raise(Failure(Format.sprintf "Method %s in %s returns %s despite being declared returning %s" mname kname r t))
        | _, _ -> () in

    let build_returnstmt opt_expr ret_env =
        let ret_val = opt_eval opt_expr ret_env in
        let ret_type = match ret_val with Some(t, _) -> Some(t) | _ -> None in
        check_ret_type ret_type;
        Sast.Return(ret_val, ret_env) in
    let build_exprstmt expr expr_env = Sast.Expr(eval' expr_env expr, expr_env) in
    let build_superstmt expr_list super_env =
        let arglist = eval_exprlist super_env expr_list in
        let init = get_superinit kname arglist in
        match map_lookup kname klass_data.parents with
            | None -> raise(Failure("Error -- getting parent for object without parent: " ^ kname))
            | Some(parent) -> Sast.Super(arglist, init.uid, parent, super_env) in

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
    Change inits so that they return this
  *)
let init_returns (func : Sast.func_def) =
    let body = if func.builtin then [] else func.body @ [Sast.Return(None, empty_environment)] in
    let this_val = (current_class, Sast.This) in
    let return_this (stmt : Sast.sstmt) = match stmt with
        | Return(None, env) -> Return(Some(this_val), env)
        | _ -> stmt in
    { func with
        returns = Some(func.inklass);
        body = List.map return_this body }

let rec update_current_ref_stmts (kname : string) (stmts : Sast.sstmt list) : Sast.sstmt list = List.map (update_current_ref_stmt kname) stmts
and update_current_ref_exprs (kname : string) (exprs : Sast.expr list) = List.map (update_current_ref_expr kname) exprs
and update_current_ref_stmt (kname : string) (stmt : Sast.sstmt) = match stmt with
    | Sast.Decl(vdef, None, env) -> Sast.Decl(vdef, None, env)
    | Sast.Decl(vdef, Some(expr), env) -> Sast.Decl(vdef, Some(update_current_ref_expr kname expr), env)
    | Sast.Expr(expr, env) -> Sast.Expr(update_current_ref_expr kname expr, env)
    | Sast.If(pieces, env) -> Sast.If(update_current_ref_clauses kname pieces, env)
    | Sast.While(expr, body, env) -> Sast.While(update_current_ref_expr kname expr, update_current_ref_stmts kname body, env)
    | Sast.Return(None, env) -> Sast.Return(None, env)
    | Sast.Return(Some(expr), env) -> Sast.Return(Some(update_current_ref_expr kname expr), env)
    | Sast.Super(args, uid, parent, env) -> Sast.Super(update_current_ref_exprs kname args, uid, parent, env)
and update_current_ref_expr (kname : string) ((atype, detail) : string * Sast.expr_detail) : string * Sast.expr_detail =
    let cleaned = match detail with
        | Sast.This -> Sast.This
        | Sast.Null -> Sast.Null
        | Sast.Id(i) -> Sast.Id(i)
        | Sast.NewObj(klass, args, uid) -> Sast.NewObj(klass, update_current_ref_exprs kname args, uid)
        | Sast.Anonymous(klass, args, refs) -> Sast.Anonymous(klass, args, refs)
        | Sast.Literal(lit) -> Sast.Literal(lit)
        | Sast.Assign(mem, data) -> Sast.Assign(update_current_ref_expr kname mem, update_current_ref_expr kname data)
        | Sast.Deref(arr, idx) -> Sast.Deref(update_current_ref_expr kname arr, update_current_ref_expr kname idx)
        | Sast.Field(expr, member) -> Sast.Field(update_current_ref_expr kname expr, member)
        | Sast.Invoc(expr, meth, args, id) -> Sast.Invoc(update_current_ref_expr kname expr, meth, update_current_ref_exprs kname args, id)
        | Sast.Unop(op, expr) -> Sast.Unop(op, update_current_ref_expr kname expr)
        | Sast.Binop(l, op, r) -> Sast.Binop(update_current_ref_expr kname l, op, update_current_ref_expr kname r)
        | Sast.Refine(refine, args, ret, switch) -> Sast.Refine(refine, update_current_ref_exprs kname args, ret, switch)
       | Sast.Refinable(refine, switch) -> Sast.Refinable(refine, switch) in
    let realtype : string = if current_class = atype then kname else atype in
    (realtype, cleaned)
and update_current_ref_clauses (kname : string) pieces =
    let (preds, bodies) = List.split pieces in
    let preds = List.map (function None -> None | Some(expr) -> Some(update_current_ref_expr kname expr)) preds in
    let bodies = List.map (update_current_ref_stmts kname) bodies in
    List.map2 (fun a b -> (a, b)) preds bodies

(**
    Given a class_data record, an Ast.func_def, an an initial environment,
    convert the func_def to a Sast.func_def. Can raise failure when there
    are issues with the statements / expressions in the function.
    @param klass_data A class_data record
    @param func An Ast.func_def to transform
    @param initial_env The initial environment
    @return A Sast.func_def value
  *)
let ast_func_to_sast_func klass_data (func : Ast.func_def) initial_env isinit =
    let with_params = List.fold_left (fun env vdef -> env_update Local vdef env) initial_env func.formals in
    let checked : Sast.sstmt list = attach_bindings klass_data func.inklass func.name func.returns func.static func.body with_params in
    let cleaned = update_current_ref_stmts func.inklass checked in
    let sast_func : Sast.func_def =
        {   returns = func.returns;
            host = func.host;
            name = func.name;
            formals = func.formals;
            static = func.static;
            body = cleaned;
            section = func.section;
            inklass = func.inklass;
            uid = func.uid;
            builtin = func.builtin } in
    let isvoid = match func.returns with None -> true | _ -> false in
    if not func.builtin && not isvoid && not (does_return_stmts func.body)
        then raise(Failure(Format.sprintf "The function %s in %s does not return on all execution paths" (full_signature_string func) func.inklass))
        else if isinit then init_returns sast_func else sast_func

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
    let change isinit func = ast_func_to_sast_func klass_data func initial_env isinit in
    let transformed : Sast.member_def = match mem with
        | Ast.VarMem(v) -> Sast.VarMem(v)
        | Ast.MethodMem(m) -> Sast.MethodMem(change false m)
        | Ast.InitMem(m) -> Sast.InitMem(change true m) in
    transformed

let init_calls_super (aklass : Sast.class_def) =
    let validate_init func_def = match func_def.builtin, func_def.body with
        | true, _ -> true
        | _, (Super(_,_,_,_)::_) -> true
        | _, _ -> false in
    let grab_init = function
        | InitMem(m) -> Some(m)
        | _ -> None in
    let get_inits mems = Util.filter_option (List.map grab_init mems) in
    let s = aklass.sections in
    let inits = List.flatten (List.map get_inits [s.publics; s.protects; s.privates]) in
    List.for_all validate_init inits

let check_main (func : Ast.func_def) = match func.formals with
    | [("System", _); ("String[]", _)] -> func
    | _ -> raise(Failure(Format.sprintf "Main functions can only have two arguments: A system (first) and an array of strings (second). -- error in %s" func.inklass))

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
    let env = update_env empty_environment Privates ast_klass in

    let mems = List.map (fun m -> ast_mem_to_sast_mem klass_data m env) in
    let funs = List.map (fun f -> ast_func_to_sast_func klass_data f env false) in

    let sections : Sast.class_sections_def =
        {   publics = mems s.publics;
            protects = mems s.protects;
            privates = mems s.privates;
            refines = funs s.refines;
            mains = funs (List.map check_main s.mains) } in

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
