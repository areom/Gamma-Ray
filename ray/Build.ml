open Ast
open Sast
open Klass
open Str
open StringModules

let env = StringMap.empty

(*ADD MORE CHECKS*)

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

let getPubMethodType klass_data kname recvr methd arglist =
  let argtypes = List.map fst arglist in
  let section = [Ast.Publics] in
  match best_method klass_data recvr methd argtypes section, recvr with
    | None, "Object" -> raise(Failure "Method not found")
    | None, _ -> getAncestor klass_data recvr methd argtypes section
    | Some(fdef), _ -> getRetType fdef

let getInstanceMethodType klass_data kname recvr methd arglist =
  let argtypes = List.map fst arglist in
  let section = [Ast.Privates; Ast.Protects; Ast.Publics] in
  match best_method klass_data recvr methd argtypes section, recvr with
    | None, "Object" -> raise(Failure "Method not found")
    | None, _ -> getAncestor klass_data recvr methd argtypes (List.tl section)
    | Some(fdef), _ -> getRetType fdef

let rec eval klass_data kname env exp =
  let eval' = eval klass_data in
  let eval_exprlist env' elist = List.map (eval' kname env') elist in
  match exp with
    | Ast.This -> (current_class, Sast.This)
    | Ast.Null -> (null_class, Sast.Null)
    | Ast.Id(vname) -> (getIDType vname env klass_data kname, Sast.Id(vname))
    | Ast.Literal(lit) -> (getLiteralType lit, Sast.Literal(lit))
    | Ast.NewObj(s1, elist) -> (s1, Sast.NewObj(s1, eval_exprlist env elist))
    | Ast.Field(expr, mbr) ->
      let (recvr_type, _) as recvr = eval' kname env expr in
      (getFieldType recvr_type mbr klass_data kname, Sast.Field(recvr, mbr))
    | Ast.Invoc(expr, methd, elist) ->
      let (recvr_type, _) as recvr = eval' kname env expr in
      let arglist = eval_exprlist env elist in
      let mtype = if recvr_type = current_class
        then getInstanceMethodType klass_data kname recvr_type methd arglist
        else getPubMethodType klass_data kname recvr_type methd arglist in
        (mtype, Sast.Invoc(recvr, methd, arglist))
    | Ast.Assign(e1, e2) ->
      let t1 = eval' kname env e1 and t2 = eval' kname env e2 in
      let type1 = fst(t1) and type2 = fst(t2) in
      if (is_subtype klass_data type2 type1 = true)
        then (type1, Sast.Assign(t1, t2))
        else raise (Failure "Assigning to incompatible type")
    | Ast.Binop(e1,op,e2) ->
      let isCompatible typ1 typ2 =
        if is_subtype klass_data typ1 typ2 then typ2
        else if is_subtype klass_data typ2 typ1 then typ1
        else raise (Failure "Binop takes incompatible types") in
      let t1 = eval' kname env e1 and t2 = eval' kname env e2 in
      let gettype op (typ1,_) (typ2,_) = match op with
        | Ast.Arithmetic(_) -> isCompatible typ1 typ2
        | Ast.NumTest(_)
        | Ast.CombTest(_) -> ignore(isCompatible typ1 typ2); "Boolean" in
      (gettype op t1 t2, Sast.Binop(t1,op,t2))
    | Ast.Refine(s1, elist, soption) ->
      let arglist = eval_exprlist env elist in
      let refinedtype = match soption with
        | Some (typ) -> typ
        | None -> "Void" in (*getMethodType env klass_data kname s1 arglist*)
      (refinedtype, Sast.Refine(s1, arglist, soption))
    | Ast.Deref(e1, e2) ->
      let expectArray typename = match last_chars typename 2 with
        | "[]" ->  List.hd (split (regexp "\\[") typename)
        | _  -> raise (Failure "Not an array type") in
      let t1 = eval' kname env e1 and t2 = eval' kname env e2 in
      let getArrayType (typ1, _) (typ2, _) =
        if typ2 = "Integer" then expectArray typ1 else raise(Failure "Dereferencing invalid") in
      (getArrayType t1 t2, Sast.Deref(t1, t2))
    | Ast.Refinable(s1) -> ("Boolean", Sast.Refinable(s1)) (*Check if the method is refinable ?*)
    | Ast.Unop(op, expr) -> let t1 = eval' kname env expr in
      ("Boolean", Sast.Unop(op,t1))
    | _ -> "Dummy",Sast.Null



(*
 * attach_bindings : Build the Sast, by annotating the expressions with type and statements with env
 * klass_data : global class data record -> type: class_data
 * kname : class name -> type: string
 * stmts : list of Ast statements inside a member function of kname -> type: Ast.stmt list
 * env : map of var declarations visible in the current scope   - > type: environment
*)
let rec attach_bindings klass_data kname stmts env =
  let eval_exprlist env' elist = List.map (eval klass_data kname env') elist in
  (* build_ifstmt iflist env ->
     iflist: Ast.if
     env : enviroment in its scope
     Builds a Sast If, env -> evaluates expressions and annotates type
     binds env to the statement list
  *)
  let build_ifstmt iflist env =
    let build_block env (exp, slist) =
      let exprtyp = match exp with
        | None -> None
        | Some exp -> let checktype (typ, exp) =
          if typ = "Boolean" then (typ, exp)
          else raise (Failure "Predicates must be boolean") in
          Some(checktype (eval klass_data kname env exp)) in
      (exprtyp, attach_bindings klass_data kname slist env) in
    Sast.If(List.map (build_block env) iflist, env) in

(*
 * Build the environment (actually Sast) for every Ast statement,
 * build the corressponding Sast.ssmt which is Ast.stmt * env
 * while updating the env in its scope if there was a new declaration
 * and for every Ast.expr, annotate it with type -> type, Ast.expr
 *)
  let build_env (output, env) stmt = match stmt with
    | Ast.While(expr, slist) ->
      let exprtyp =
        let e1 = eval klass_data kname env expr in
        match fst(e1) with
          | "Boolean" -> e1
          | _ -> raise (Failure "While expects Boolean") in
      (Sast.While(((exprtyp), attach_bindings klass_data kname slist env), env)::output, env)
    | Ast.If (iflist) -> ((build_ifstmt iflist env)::output, env)
    | Ast.Decl((vtype,vname),opt_expr) ->
      let sastexpr = match opt_expr with
        | Some exp -> Some(eval klass_data kname env exp)
        | None -> None in
      (Sast.Decl((vtype, vname), sastexpr , env)::output, (StringMap.add vname (vtype,Local) env))
    | Ast.Expr(expr) -> (Sast.Expr((eval klass_data kname env expr), env)::output, env)
    | Ast.Return(opt_expr) ->
      let sastexpr = match opt_expr with
        | Some exp -> Some (eval klass_data kname env exp)
        | None -> None in
      (Sast.Return(sastexpr, env)::output, env)
    | Ast.Super(expr_list) -> (Sast.Super(eval_exprlist env expr_list,env)::output, env) in
  (List.rev (fst(List.fold_left build_env ([],env) stmts)))

