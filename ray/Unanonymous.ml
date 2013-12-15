open StringModules
open Sast
open Ast
open Util

(** Take a collection of Sast class_defs and deanonymize them. *)


(** The data needed to deanonymize a list of classes and store the results. *)
type anon_state = {
    labeler : int lookup_map ;      (** Label deanonymized classes *)
    deanon : Ast.class_def list ;   (** List of Ast.class_def classes that are deanonymized. *)
    clean : Sast.class_def list ;   (** List of clean Sast.class_def classes *)
    data : GlobalData.class_data ;  (** A class_data record used for typing *)
    current : string ;              (** The class that is currently being examined *)
}

(**
    Given the initial anon_state, an environment, and an expr_detail, remove all
    anonymous object instantiations from the expr and replace them with the
    instantiation of a newly constructed class. This returns a changed expr_detail
    value and an updated state -- i.e. maybe a new ast class is added to it.
    @param init_state anon_state value
    @param env an environment (like those attached to statements in sAST)
    @param expr_deets an expr_detail to transform
    @return (new expr detail, updated state)
  *)
let deanon_expr_detail init_state env expr_deets =
    let get_label state klass =
        let (n, labeler) = match map_lookup klass state.labeler with
            | None -> (0, StringMap.add klass 0 state.labeler)
            | Some(n) -> (n+1, StringMap.add klass (n+1) state.labeler) in
        (Format.sprintf "anon_%s_%d" klass n, { state with labeler = labeler }) in

    let get_var_type state env var_name =
        match map_lookup var_name env with
            | Some(vinfo) -> Some(fst vinfo)
            | None -> match Klass.class_field_lookup state.data state.current var_name with
                | Some((_, vtype, _)) -> Some(vtype)
                | _ -> None in

    let deanon_init formals klass : Ast.func_def =
        let assigner (_, vname) = Ast.Expr(Ast.Assign(Ast.Field(Ast.This, vname), Ast.Id(vname))) in
        {   returns = None;
            host = None;
            name = "init";
            static = false;
            formals = formals;
            body = List.map assigner formals;
            section = Publics;
            inklass = klass;
            uid = UID.uid_counter ();
            builtin = false } in

    let deanon_klass freedefs klass parent refines =
        let init = deanon_init freedefs klass in
        let vars = List.map (fun vdef -> Ast.VarMem(vdef)) freedefs in
        let sections =
            {   privates = vars;
                protects = [];
                publics = [InitMem(init)];
                refines = refines;
                mains = []; } in
        let theklass =
            {   klass = klass;
                parent = Some(parent);
                sections = sections } in
        (init.uid, theklass) in

    let deanon_freedefs state env funcs =
        let freeset = Variables.free_vars_funcs StringSet.empty funcs in
        let freevars = List.sort compare (StringSet.elements freeset) in

        let none_snd = function
          | (None, v) -> Some(v)
          | _ -> None in
        let some_fst = function
          | (Some(t), v) -> Some((t, v))
          | _ -> None in
        let add_type v = (get_var_type state env v, v) in

        let typed = List.map add_type freevars in
        let unknowns = List.map none_snd typed in
        let knowns = List.map some_fst typed in

        match Util.filter_option unknowns with
            | [] -> Util.filter_option knowns
            | vs -> raise(Failure("Unknown variables " ^ String.concat ", " vs ^ " within anonymous object definition.")) in

    match expr_deets with
        | Sast.Anonymous(klass, args, refines) ->
            let (newklass, state) = get_label init_state klass in
            let freedefs = deanon_freedefs state env refines in
            let (init_id, ast_class) = deanon_klass freedefs newklass klass refines in
            let args = List.map (fun (t, v) -> (t, Sast.Id(v))) freedefs in
            let instance = Sast.NewObj(newklass, args, init_id) in
            let state = { state with deanon = ast_class::state.deanon } in
            (instance, state)
        | _ -> (expr_deets, init_state)

(**
    Update an type-tagged sAST expression to be deanonymized.
    Returns the deanonymized expr and a possibly updated anon_state
    @param init_state anon_state value
    @param env an environment like those attached to stmts in the sAST
    @param t the type of the expr_detail exp
    @param exp an expression detail
    @return ((t, exp'), state') where exp' is exp but deanonymized and
    state' is an updated version of init_state
  *)
let deanon_expr init_state env (t, exp) =
    let (deets, state) = deanon_expr_detail init_state env exp in
    ((t, deets), state)

(**
    Deanonymize a list of expressions maintaining the state properly throughout.
    Returns the list of expressions (deanonymized) and the updated state.
    @param init_state an anon_state value
    @param env an environment like those attached to statments (sAST)
    @param list a list of expressions (sAST exprs)
    @return (list', state') where list' is the deanonymized list and
    state' is the updated state
  *)
let deanon_exprs init_state env list =
    let folder (rexprs, state) expr =
        let (deets, state) = deanon_expr state env expr in
        (deets::rexprs, state) in
    let (rexprs, state) = List.fold_left folder ([], init_state) list in
    (List.rev rexprs, state)

(**
    Deanonymize a statement.
    Returns the deanonymized statement and the updated state.
    @param input_state an anon_state value
    @param stmt a statement to deanonymize
    @return (stmt', state') the statement and state, updated.
  *)
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

    let deanon_super init_state env args init_id =
        let (deets, state) = deanon_exprs init_state env args in
        (Sast.Super(deets, init_id, env), state) in

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
        | Sast.Super(args, init_id, env) -> deanon_super input_state env args init_id

(**
    Update an entire list of statements to be deanonymized.
    Maintains the update to the state throughout the computation.
    Returns a deanonymized list of statements and an updated state.
    @param init_state an anon_state value
    @param stmts a list of statements
    @return (stmts', state') the updated statements and state
  *)
and deanon_stmts init_state stmts =
    let folder (rstmts, state) stmt =
        let (stmt, state) = deanon_stmt state stmt in
        (stmt::rstmts, state) in
    let (rstmts, state) = List.fold_left folder ([], init_state) stmts in
    (List.rev rstmts, state)

(**
    Deanonymize the body of a function.
    Return the updated function and updated state.
    @param init_state an anon_state value
    @param func a func_def (sAST)
    @return (func', state') the updated function and state
  *)
let deanon_func init_state (func : Sast.func_def) =
    let (stmts, state) = deanon_stmts init_state func.body in
    ({ func with body = stmts }, state)

(**
    Deanonymize an entire list of functions, threading the state
    throughout and maintaining the changes. Returns the list of
    functions, updated, and the updated state.
    @param init_state an anon_state value
    @param funcs a list of functions
    @return (funcs', state') the updated functions and state
  *)
let deanon_funcs init_state funcs =
    let folder (rfuncs, state) func =
        let (func, state) = deanon_func state func in
        (func::rfuncs, state) in
    let (funcs, state) = List.fold_left folder ([], init_state) funcs in
    (List.rev funcs, state)

(**
    Deanonymize an Sast member_def
    Returns the deanonymized member and a possibly updated state.
    @param init_state an anon_state value
    @param mem a member to deanonymize
    @return (mem', state') the updated member and state
  *)
let deanon_member init_state mem = match mem with
    | Sast.MethodMem(f) ->
        let (func, state) = deanon_func init_state f in
        (Sast.MethodMem(func), state)
    | Sast.InitMem(f) ->
        let (func, state) = deanon_func init_state f in
        (Sast.InitMem(func), state)
    | mem -> (mem, init_state)

(**
    Deanonymize a list of members.  Return the deanonymized list
    and a possibly updated state.
    @param init_state an anon_state value
    @param members a list of members to deanonymize
    @return (mems', state') the updated members and state
  *)
let deanon_memlist (init_state : anon_state) (members : Sast.member_def list) : (Sast.member_def list * anon_state) =
    let folder (rmems, state) mem =
        let (mem, state) = deanon_member state mem in
        (mem::rmems, state) in
    let (rmems, state) = List.fold_left folder ([], init_state) members in
    (List.rev rmems, state)

(**
    Deanonymize an entire class. Return the deanonymized class
    and an updated state.
    @param init_state an anon_state value
    @param aklass an sAST class to deanonymize
    @return (class', state') the udpated class and state.
  *)
let deanon_class init_state (aklass : Sast.class_def) =
    let s = aklass.sections in
    let state = { init_state with current = aklass.klass } in
    let (publics, state) = deanon_memlist state s.publics in
    let (protects, state) = deanon_memlist state s.protects in
    let (privates, state) = deanon_memlist state s.privates in
    let (refines, state) = deanon_funcs state s.refines in
    let (mains, state) = deanon_funcs state s.mains in
    let sections : Sast.class_sections_def =
        {   publics = publics;
            protects = protects;
            privates = privates;
            refines = refines;
            mains = mains } in
    let cleaned = { aklass with sections = sections } in
    (state.deanon, { state with clean = cleaned::state.clean; current = ""; deanon = [] })

(** A startng state for deanonymization. *)
let empty_deanon_state data =
    {   labeler = StringMap.empty;
        deanon = [];
        clean = [];
        data = data;
        current = ""; }

(**
    Given global class information and parsed and tagged classes,
    deanonymize the classes. This will add more classes to the
    global data, which will be updated accordingly.
    @param klass_data global class_data info
    @param sast_klasses tagged sAST class list
    @return If everything goes okay with updating the global data
    for each deanonymization, then left((state', data')) will be
    returned where state' contains all (including newly created)
    sAST classes in its clean list and data' has been updated to
    reflect any new classes. If anything goes wrong, Right(issue)
    is returned, where the issue is just as in building the global
    class_data info to begin with, but now specific to what goes
    on in deanonymization (i.e. restricted to those restricted
    classes themselves).
  *)
let deanonymize klass_data sast_klasses =
    let is_empty = function
        | [] -> true
        | _ -> false in

    let rec run_deanon init_state asts sasts = match asts, sasts with
        (* Every sAST has been deanonymized, even the deanonymized ones converted into sASTs
         * Every Ast has been sAST'd too. So we are done.
         *)
        | [], [] ->
            if is_empty init_state.deanon then Left((init_state.data, init_state.clean))
            else raise(Failure("Deanonymization somehow did not recurse properly."))

        | [], klass::rest ->
            let (asts, state) = deanon_class init_state klass in
            run_deanon state asts rest

        | klass::rest, _ -> match KlassData.append_leaf init_state.data klass with
            | Left(data) ->
                let sast_klass = BuildSast.ast_to_sast data klass in
                let state = { init_state with data = data } in
                run_deanon state rest (sast_klass::sasts)
            | Right(issue) -> Right(issue) in

    run_deanon (empty_deanon_state klass_data) [] sast_klasses
