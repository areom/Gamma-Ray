open StringModules
open Sast
open Ast
open Util

(**
  Take a collection of Sast class_defs and deanonymize them --
  This means we need to go from [Sast.class_def] -> class_data -> [Ast.class_def] * [Sast.class_def]
  The first item in the return are the deanonymized classes; the second item is the updated input list
  (i.e. the same as the param but with the anonymous objects removed).
 *)

type anon_state = {
  labeler : int lookup_map ;
  deanon : Ast.class_def list ;
  clean : Sast.class_def list
}

let get_label klass state =
  let (n, labeler) = match map_lookup klass state.labeler with
    | None -> (0, StringMap.add klass 0 state.labeler)
    | Some(n) -> (n+1, StringMap.add klass (n+1) state.labeler) in
  (Format.sprintf "anon_%s_%d" klass n, { state with labeler = labeler })

let deanon_expr_detail init_state env expr_deets =
  let deanon_init formals klass : Ast.func_def =
    let assigner (_, vname) = Ast.Expr(Ast.Assign(Ast.Field(Ast.This, vname), Ast.Id(vname))) in
      { returns = None;
        host = None;
        name = "init";
        static = false;
        formals = formals;
        body = List.map assigner formals;
        section = Publics;
        inklass = klass; } in

    let deanon_klass freedefs klass parent refines =
      { klass = klass;
        parent = Some(parent);
        sections =
        { privates = List.map (fun vdef -> Ast.VarMem(vdef)) freedefs;
          protects = [];
          publics = [InitMem(deanon_init freedefs klass)];
          refines = refines;
          mains = []; } } in

  let deanon_freedefs funcs env =
    let freeset = FreeVariables.free_vars_funcs StringSet.empty funcs in
    let freevars = List.sort compare (StringSet.elements freeset) in
    let unknown = List.filter (fun v -> not (StringMap.mem v env)) freevars in
    let add_type v = let (t, _) = StringMap.find v env in (t, v) in
    match unknown with
      | [] -> List.map add_type freevars
      | _ -> raise(Failure("Unknown variables " ^ String.concat ", " unknown ^ " within anonymous object definition.")) in

  match expr_deets with
    | Sast.Anonymous(klass, args, refines) ->
      let (newklass, state) = get_label klass init_state in
      let freedefs = deanon_freedefs refines env in
      let ast_class = deanon_klass freedefs newklass klass refines in
      let args = List.map (fun (t, v) -> (t, Sast.Id(v))) freedefs in
      let instance = Sast.NewObj(newklass, args) in
      let state = { state with deanon = ast_class::state.deanon } in
      (instance, state)
    | _ -> (expr_deets, init_state)

let deanon_expr init_state env (t, exp) = 
  let (deets, state) = deanon_expr_detail init_state env exp in
  ((t, deets), state)

let deanon_exprs init_state env list =
  let folder (rexprs, state) expr =
    let (deets, state) = deanon_expr state env expr in
    (deets::rexprs, state) in
  let (rexprs, state) = List.fold_left folder ([], init_state) list in
  (List.rev rexprs, state)


(* Clean up the expressions at the top level of a statement *)
let rec deanon_stmt input_state stmt =
  let deanon_decl init_state env = function
    | (vdef, Some(expr)) ->
      let (deets, state) = deanon_expr init_state env expr in
      (Sast.Decl(vdef, Some(deets), env), state)
    | (vdef, _) -> (Sast.Decl(vdef, None, env), init_state) in

  let deanon_exprstmt init_state env expr =
    let (deets, state) = deanon_expr init_state env expr in
    (Sast.Expr(deets, env), state) in

  let deanon_return init_state env = function
    | None -> (Sast.Return(None, env), init_state)
    | Some(expr) ->
      let (deets, state) = deanon_expr init_state env expr in
      (Sast.Return(Some(deets), env), state) in

  let deanon_super init_state env args =
    let (deets, state) = deanon_exprs init_state env args in
    (Sast.Super(deets, env), state) in

  let deanon_while init_state env (expr, stmts) =
    let (test, state) = deanon_expr init_state env expr in
    let (body, state) = deanon_stmts state stmts in
    (Sast.While(test, body, env), state) in

  let deanon_if init_state env pieces =
    let folder (rpieces, state) piece =
      let (piece, state) = match piece with
        | (None, stmts) ->
          let (body, state) = deanon_stmts state stmts in
          ((None, body), state)
        | (Some(expr), stmts) ->
          let (test, state) = deanon_expr state env expr in
          let (body, state) = deanon_stmts state stmts in
          ((Some(test), body), state) in
      (piece::rpieces, state) in
    let (rpieces, state) = List.fold_left folder ([], init_state) pieces in
    (Sast.If(List.rev rpieces, env), state) in

  match stmt with
    | Sast.Decl(vdef, opt_expr, env) -> deanon_decl input_state env (vdef, opt_expr)
    | Sast.If(pieces, env) -> deanon_if input_state env pieces
    | Sast.While(test, body, env) -> deanon_while input_state env (test, body)
    | Sast.Expr(expr, env) -> deanon_exprstmt input_state env expr
    | Sast.Return(opt_expr, env) -> deanon_return input_state env opt_expr
    | Sast.Super(args, env) -> deanon_super input_state env args

and deanon_stmts init_state stmts =
  let folder (rstmts, state) stmt =
    let (stmt, state) = deanon_stmt state stmt in
    (stmt::rstmts, state) in
  let (rstmts, state) = List.fold_left folder ([], init_state) stmts in
  (List.rev rstmts, state)

let deanon_func init_state (func : Sast.func_def) =
  let (stmts, state) = deanon_stmts init_state func.body in
  ({ func with body = stmts }, state)

let deanon_funcs init_state funcs =
  let folder (rfuncs, state) func =
    let (func, state) = deanon_func state func in
    (func::rfuncs, state) in
  let (funcs, state) = List.fold_left folder ([], init_state) funcs in
  (List.rev funcs, state)

let deanon_member init_state = function
  | Sast.MethodMem(f) ->
    let (func, state) = deanon_func init_state f in
    (Sast.MethodMem(func), state)
  | Sast.InitMem(f) ->
    let (func, state) = deanon_func init_state f in
    (Sast.InitMem(func), state)
  | mem -> (mem, init_state)

let deanon_memlist (init_state : anon_state) (members : Sast.member_def list) : (Sast.member_def list * anon_state) =
  let folder (rmems, state) mem =
    let (mem, state) = deanon_member state mem in
    (mem::rmems, state) in
  let (rmems, state) = List.fold_left folder ([], init_state) members in
  (List.rev rmems, state)

let deanon_class init_state (aklass : Sast.class_def) =
  let s = aklass.sections in
  let (publics, state) = deanon_memlist init_state s.publics in
  let (protects, state) = deanon_memlist state s.protects in
  let (privates, state) = deanon_memlist state s.privates in
  let (refines, state) = deanon_funcs state s.refines in
  let (mains, state) = deanon_funcs state s.mains in
  let sections : Sast.class_sections_def = {
    publics = publics;
    protects = protects;
    privates = privates;
    refines = refines;
    mains = mains
  } in
  let cleaned = { aklass with sections = sections } in
  { state with clean = cleaned::state.clean }

let empty_deanon_state = {
  labeler = StringMap.empty;
  deanon = [];
  clean = [];
}

let deanonymize klass_data sast_klasses =
  let rec run_deanon init_state data asts sasts = match asts, sasts with
    | [], [] -> Left((init_state, data))
    | klass::rest, _ -> (match Klass.append_leaf_class_node data klass with
      | Left(data) ->
        let sast_klass = Build.ast_to_sast data klass in
        run_deanon init_state data rest (sast_klass::sasts)
      | Right(issue) -> Right(issue))
    | _, klass::rest ->
      let state = deanon_class init_state klass in
      run_deanon state data state.deanon rest in
  run_deanon empty_deanon_state klass_data [] sast_klasses
