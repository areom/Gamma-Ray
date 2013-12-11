open Ast
open Sast
open Klass
open Str
open StringModules

let env = StringMap.empty

(*ADD MORE CHECKS*)

let env_update mode (vtype, vname) = StringMap.add vname (vtype, mode)

let current_class = "_CurrentClassMarker_"
let null_class = "_Null_"

(**
    Get the type of an instance variable data for a given variable name
    and class name given a class_data record.
    @param vname The variable being sought
    @param klass_data The class_data record
    @param kname The class we are looking in
    @return Either Some(section, klass) where vname is an instance variable
    in the variable table (map) for the class (klass), or None if there is no
    variable to be found.
  *)
let rec getInstanceType vname klass_data kname =
  match class_var_lookup klass_data kname vname, kname with
    | Some(varmap), _ -> Some(varmap, kname)
    | None, "Object" -> None
    | _, _ -> getInstanceType vname klass_data (StringMap.find kname klass_data.parents)

(**
    Get an Id's type - Not accessed through objects
    Therefore, it can be a local variable visible in the current scope i.e., present inside env
    It can be an instance variable in the current class of any access scope
    Or it can be an instance variable which is either protected or public in any of its ancestor
    We just get to the closest ancestor
  *)
let getIDType vname env klass_data kname =
  match map_lookup vname env with
    | Some((vtyp, _)) -> vtyp
    | _ -> match getInstanceType vname klass_data kname with
      | Some((section, vtyp), cname) ->
        if (kname = cname || section <> Ast.Privates) then vtyp
        else raise (Failure("ID " ^ vname ^ " not in accessible scope"))
      | None -> raise (Failure("ID " ^ vname ^ " not found"))

(* Do a lookup on the instance variable for the current classdef and return
   its type else then recurse its ancestor *)
let getFieldType recvr member klass_data cur_kname =
  let lookupclass = if recvr = current_class then cur_kname else recvr in
  match getInstanceType member klass_data lookupclass with
    | Some((section, vtyp), cname) ->
      if section = Ast.Publics then vtyp
      else if recvr = current_class then
        if section = Ast.Protects then vtyp
        else if section = Ast.Privates && cname = cur_kname then vtyp
        else raise (Failure "Non-public members can only this as receiver")
     else raise (Failure "Access only public through instance")
    | None -> raise (Failure "Field unknown")

let getLiteralType litparam = match litparam with
  | Ast.Int(i) -> "Integer"
  | Ast.Float(f) -> "Float"
  | Ast.String(s) -> "String"
  | Ast.Bool(b) -> "Boolean"

let getRetType (fdef : Ast.func_def) = match fdef.returns with
  | Some(retval) -> retval
  | None -> "Void"

let rec getAncestor klass_data recvr methd argtypelist section =
  let parent = StringMap.find recvr klass_data.parents in
  match best_method klass_data parent methd argtypelist section, parent with
    | None, "Object" -> raise(Failure "Method not found")
    | None, _ -> getAncestor klass_data parent methd argtypelist section
    | Some(fdef), _ -> getRetType fdef

let getPubMethodType klass_data recvr methd arglist =
  let argtypes = List.map fst arglist in
  let section = [Ast.Publics] in
  match best_method klass_data recvr methd argtypes section, recvr with
    | None, "Object" -> raise(Failure "Method not found")
    | None, _ -> getAncestor klass_data recvr methd argtypes section
    | Some(fdef), _ -> getRetType fdef

let getInstanceMethodType klass_data recvr methd arglist =
  let argtypes = List.map fst arglist in
  let section = [Ast.Privates; Ast.Protects; Ast.Publics] in
  match best_method klass_data recvr methd argtypes section, recvr with
    | None, "Object" -> raise(Failure "Method not found")
    | None, _ -> getAncestor klass_data recvr methd argtypes (List.tl section)
    | Some(fdef), _ -> getRetType fdef

let rec eval klass_data kname env exp =
  let eval' expr = eval klass_data kname env expr in
  let eval_exprlist elist = List.map eval' elist in

  let get_field expr mbr =
    let (recvr_type, _) as recvr = eval' expr in
    (getFieldType recvr_type mbr klass_data kname, Sast.Field(recvr, mbr)) in

  let get_invoc expr methd elist =
    let (recvr_type, _) as recvr = eval' expr in
    let arglist = eval_exprlist elist in
    let mtype = if recvr_type = current_class
      then getInstanceMethodType klass_data recvr_type methd arglist
      else getPubMethodType klass_data recvr_type methd arglist in
      (mtype, Sast.Invoc(recvr, methd, arglist)) in

  let get_assign e1 e2 =
    let (t1, t2) = (eval' e1, eval' e2) in
    let (type1, type2) = (fst t1, fst t2) in
    if is_subtype klass_data type2 type1
      then (type1, Sast.Assign(t1, t2))
      else raise (Failure "Assigning to incompatible type") in

  let get_binop e1 op e2 =
    let isCompatible typ1 typ2 =
      if is_subtype klass_data typ1 typ2 then typ2
      else if is_subtype klass_data typ2 typ1 then typ1
      else raise (Failure "Binop takes incompatible types") in
    let (t1, t2) = (eval' e1, eval' e2) in
    let gettype op (typ1,_) (typ2,_) = match op with
      | Ast.Arithmetic(Neg) -> raise(Failure("Negation is not a binary operation!"))
      | Ast.CombTest(Not) -> raise(Failure("Boolean negation is not a binary operation!"))
      | Ast.Arithmetic(_) -> isCompatible typ1 typ2
      | Ast.NumTest(_)
      | Ast.CombTest(_) -> ignore(isCompatible typ1 typ2); "Boolean" in
    (gettype op t1 t2, Sast.Binop(t1,op,t2)) in

  let get_refine s1 elist soption =
    let arglist = eval_exprlist elist in
    let refinedtype = match soption with
      | Some (typ) -> typ
      | None -> "Void" in (*getMethodType env klass_data kname s1 arglist*)
    (refinedtype, Sast.Refine(s1, arglist, soption)) in

  let get_deref e1 e2 =
    let expectArray typename = match last_chars typename 2 with
      | "[]" -> first_chars typename (String.length typename - 2)
      | _  -> raise (Failure "Not an array type") in
    let (t1, t2) = (eval' e1, eval' e2) in
    let getArrayType (typ1, _) (typ2, _) = match typ2 with
      | "Integer" -> expectArray typ1
      | _ -> raise(Failure "Dereferencing invalid") in
    (getArrayType t1 t2, Sast.Deref(t1, t2)) in

  let get_unop op expr = match op with
    | Ast.Arithmetic(Neg) -> let (typ, _) as evaled = eval' expr in (typ, Sast.Unop(op, evaled))
    | Ast.CombTest(Not) -> ("Boolean", Sast.Unop(op, eval' expr))
    | _ -> raise(Failure("Unknown binary operator " ^ Inspector.inspect_op op ^ " given.")) in

  match exp with
    | Ast.This -> (current_class, Sast.This)
    | Ast.Null -> (null_class, Sast.Null)
    | Ast.Id(vname) -> (getIDType vname env klass_data kname, Sast.Id(vname))
    | Ast.Literal(lit) -> (getLiteralType lit, Sast.Literal(lit))
    | Ast.NewObj(s1, elist) -> (s1, Sast.NewObj(s1, eval_exprlist elist))
    | Ast.Field(expr, mbr) -> get_field expr mbr
    | Ast.Invoc(expr, methd, elist) -> get_invoc expr methd elist
    | Ast.Assign(e1, e2) -> get_assign e1 e2
    | Ast.Binop(e1,op,e2) -> get_binop e1 op e2
    | Ast.Refine(s1, elist, soption) -> get_refine s1 elist soption
    | Ast.Deref(e1, e2) -> get_deref e1 e2
    | Ast.Refinable(s1) -> ("Boolean", Sast.Refinable(s1)) (*Check if the method is refinable ?*)
    | Ast.Unop(op, expr) -> get_unop op expr
    | Ast.Anonymous(atype, args, body) -> (atype, Sast.Anonymous(atype, args, body)) (* Delay evaluation *)


(*
 * attach_bindings : Build the Sast, by annotating the expressions with type and statements with env
 * klass_data : global class data record -> type: class_data
 * kname : class name -> type: string
 * stmts : list of Ast statements inside a member function of kname -> type: Ast.stmt list
 * env : map of var declarations visible in the current scope   - > type: environment
*)
let rec attach_bindings klass_data kname stmts initial_env =
  (* Calls that go easy on the eyes *)
  let eval' = eval klass_data kname in
  let attach' = attach_bindings klass_data kname in
  let eval_exprlist env elist = List.map (eval' env) elist in

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

  let build_declstmt vdef opt_expr decl_env = Sast.Decl(vdef, opt_eval opt_expr decl_env, decl_env) in
  let build_returnstmt opt_expr ret_env = Sast.Return(opt_eval opt_expr ret_env, ret_env) in
  let build_exprstmt expr expr_env = Sast.Expr(eval' expr_env expr, expr_env) in
  let build_superstmt expr_list super_env = Sast.Super(eval_exprlist super_env expr_list, super_env) in

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


let ast_func_to_sast_func klass_data kname (func : Ast.func_def) initial_env =
  let with_params = List.fold_left (fun env vdef -> env_update Local vdef env) initial_env func.formals in
  let sast_func : Sast.func_def =
    { returns = func.returns;
      host = func.host;
      name = func.name;
      formals = func.formals;
      static = func.static;
      body = attach_bindings klass_data kname func.body with_params;
      section = func.section;
      inklass = func.inklass; } in
  sast_func

let ast_mem_to_sast_mem klass_data kname (mem : Ast.member_def) initial_env =
  let change func = ast_func_to_sast_func klass_data kname func initial_env in
  let transformed : Sast.member_def = match mem with
    | Ast.VarMem(v) -> Sast.VarMem(v)
    | Ast.MethodMem(m) -> Sast.MethodMem(change m)
    | Ast.InitMem(m) -> Sast.InitMem(change m) in
  transformed

let ast_to_sast klass_data (ast_klass : Ast.class_def) =
  let s : Ast.class_sections_def = ast_klass.sections in
  let env = StringMap.empty in

  let mems = List.map (fun m -> ast_mem_to_sast_mem klass_data ast_klass.klass m env) in
  let funs = List.map (fun f -> ast_func_to_sast_func klass_data ast_klass.klass f env) in

  let sections : Sast.class_sections_def =
    { publics = mems s.publics;
      protects = mems s.protects;
      privates = mems s.privates;
      refines = funs s.refines;
      mains = funs s.mains } in

  let sast_klass : Sast.class_def =
    { klass = ast_klass.klass;
      parent = ast_klass.parent;
      sections = sections } in

  sast_klass
