open Ast
open Sast
open Cast
open Klass
open StringModules
open GlobalData

let to_fname fuid fname = Format.sprintf "f_%s_%s" fuid fname
let to_rname fuid fhost fname = Format.sprintf "f_%s_%s_%s" fuid fhost fname

let get_fname (f : Sast.func_def) = to_fname f.uid f.name
let get_rname (f : Sast.func_def) = match f.host with
    | None -> raise(Failure("Generating refine name for non-refinement " ^ f.name ^ " in class " ^ f.inklass ^ "."))
    | Some(host) -> to_rname f.uid host f.name
let get_vname vname = "v_" ^ vname
let get_tname tname = "t_" ^ tname

(*Convert the sast expr to cast expr*)
let rec sast_to_castexpr env sast_expr = (fst sast_expr, c_expr_detail (snd sast_expr) env)
and sast_to_castexprlist env explist = List.map (sast_to_castexpr env) explist

(*Conver the sast expr_detail to cast_expr detail*)
and c_expr_detail sastexp env =
    match sastexp with
      Sast.This              -> Cast.This
    | Sast.Null              -> Cast.Null
    | Sast.Id(vname)         -> Cast.Id(get_vname vname, snd (StringMap.find vname env))
    | Sast.NewObj(classname, args, fuid) -> Cast.NewObj(classname, to_fname fuid "init", sast_to_castexprlist env args)
    | Sast.Literal(lit)      -> Cast.Literal(lit)
    | Sast.Assign(e1, e2)    -> Cast.Assign(sast_to_castexpr env e1, sast_to_castexpr env e2)
    | Sast.Deref(e1, e2)     -> Cast.Deref(sast_to_castexpr env e1, sast_to_castexpr env e2)
    | Sast.Field(e1, e2)     -> Cast.Field(sast_to_castexpr env e1, e2)
    | Sast.Invoc(recv, fname, args, fuid) -> Cast.Invoc(sast_to_castexpr env recv, to_fname fuid fname, sast_to_castexprlist env args)
    | Sast.Unop(op, expr)    -> Cast.Unop(op, sast_to_castexpr env expr)
    | Sast.Binop(e1, op, e2) -> Cast.Binop(sast_to_castexpr env e1, op, sast_to_castexpr env e2)
    | Sast.Refine(name, args, rtype, switch) -> Cast.Refine(name, sast_to_castexprlist env args, rtype, switch)
    | Sast.Refinable(name, switch) -> Cast.Refinable(name, switch)
    | Anonymous(_, _, _)     -> raise(Failure("Anonymous objects should have been deanonymized."))

(*Convert the statement list by invoking cstmt on each of the sast stmt*)
let rec cstmtlist slist =  List.map cstmt slist

(* Prepend suffixes *)
and cdef (vtype, vname) = (get_tname vtype, get_vname vname)

(*convert sast statement to c statements*)
and cstmt sstmt =
    let getoptexpr env optexpr =
        match optexpr with
          Some exp -> Some(sast_to_castexpr env exp)
        | None     -> None in

    let rec getiflist env iflist =
        match iflist with
              []                   -> []
            | [(optexpr, slist)]   -> [(getoptexpr env optexpr, cstmtlist slist)]
            | (optexpr, slist)::tl -> (getoptexpr env optexpr, cstmtlist slist):: getiflist env tl in

    let getsuper args fuid env =
        let recvr = ("This", Cast.This) in
        let init = to_fname fuid "init" in
        let cargs = sast_to_castexprlist env args in
        Cast.Invoc((recvr, init, cargs)) in

    match sstmt with
      Sast.Decl(var_def, optexpr, env) -> Cast.Decl(cdef var_def, getoptexpr env optexpr, env)
    | Sast.If(iflist, env)             -> Cast.If(getiflist env iflist, env)
    | Sast.While(expr, sstmtlist, env) -> Cast.While(sast_to_castexpr env expr, cstmtlist sstmtlist, env)
    | Sast.Expr(exp, env)              -> Cast.Expr(sast_to_castexpr env exp, env)
    | Sast.Return(optexpr, env)        -> Cast.Return(getoptexpr env optexpr, env)
    | Sast.Super(args, fuid, env)      -> Cast.Expr(("Void", getsuper args fuid env), env)

(**
    Trim up the sast func_def to the cast cfunc_def
    @param func It's a sast func_def. Woo.
    @return It's a cast cfunc_def. Woo.
*)
let sast_to_cast_func (func : Sast.func_def) : cfunc =
    let name = match func.host with
        | None -> get_fname func
        | Some(host) -> get_rname func in
    {   returns = func.returns;
        name = name;
        formals = func.formals;
        static = func.static;
        body = cstmtlist func.body;
        builtin = func.builtin; }

let build_class_struct_map klass_data (sast_classes : Sast.class_def list) =
    (* Extract the ancestry and variables from a class into a cdef *)
    let klass_to_struct klass_name (aklass : Ast.class_def) =
        let compare (_, n1) (_, n2) = Pervasives.compare n1 n2 in
        let ivars = List.flatten (List.map snd (Klass.klass_to_variables aklass)) in
        [(klass_name, List.sort compare ivars)] in

    (* Map each individial class to a basic class_struct *)
    let struct_map = StringMap.mapi klass_to_struct klass_data.classes in

    (* Now, assuming we get parents before children, update the maps appropriately *)
    let folder map = function
        | "Object" -> StringMap.add "Object" (StringMap.find "Object" struct_map) map
        | aklass ->
            let parent = StringMap.find (StringMap.find aklass klass_data.parents) map in
            let this = StringMap.find aklass struct_map in
            StringMap.add aklass (this @ parent) map in

    (* Update the map so that each child has information from parents *)
    let struct_map = List.fold_left folder StringMap.empty (Klass.get_class_names klass_data) in

    (* Reverse the values so that they start from the root *)
    StringMap.map List.rev struct_map

let sast_functions (klasses : Sast.class_def list) =
    (* Map a Sast class to its functions *)
    let get_functions (klass : Sast.class_def) =
        let s = klass.sections in
        let funcs = function
            | Sast.MethodMem(m) -> Some(m)
            | Sast.InitMem(i) -> Some(i)
            | _ -> None in
        let get_funcs mems = Util.filter_option (List.map funcs mems) in
        List.flatten [ get_funcs s.publics ; get_funcs s.protects ; get_funcs s.privates ; s.refines ; s.mains ] in

    let all_functions = List.flatten (List.map get_functions klasses) in
    let all_mains = List.flatten (List.map (fun k -> k.sections.mains) klasses) in

    (all_functions, all_mains)

let sast_to_cast klass_data (klasses : Sast.class_def list) : Cast.program =
    let (funcs, mains) = sast_functions klasses in
    let main_case f = (f.inklass, get_fname f) in
    let cfuncs = List.map sast_to_cast_func funcs in
    let main_switch = List.map main_case mains in
    let struct_map = build_class_struct_map klass_data klasses in

    (struct_map, cfuncs, main_switch)
