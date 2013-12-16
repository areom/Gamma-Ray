open Ast
open Sast
open Cast
open Klass
open StringModules
open GlobalData

(*Convert the sast expr to cast expr*)
let rec sast_to_castexpr sast_expr =
    (fst sast_expr, c_expr_detail (snd sast_expr) )

and sast_to_castexprlist explist  = List.map sast_to_castexpr explist

(*Conver the sast expr_detail to cast_expr detail*)
and c_expr_detail sastexp =

    match sastexp with
      Sast.This              -> Cast.This
    | Sast.Null              -> Cast.Null
    | Sast.Id(vname)         -> Cast.Id(vname)
    | Sast.NewObj(classname, args, fuid) -> Cast.NewObj(classname, "f_"^fuid^"_init"^classname ,sast_to_castexprlist args)
    | Sast.Literal(lit)      -> Cast.Literal(lit)
    | Sast.Assign(e1, e2)    -> Cast.Assign(sast_to_castexpr e1, sast_to_castexpr e2)
    | Sast.Deref(e1, e2)     -> Cast.Deref(sast_to_castexpr e1, sast_to_castexpr e2)
    | Sast.Field(e1, e2)     -> Cast.Field(sast_to_castexpr e1, e2)
    | Sast.Unop(op, expr)    -> Cast.Unop(op, sast_to_castexpr expr)
    | Sast.Binop(e1, op, e2) -> Cast.Binop(sast_to_castexpr e1, op, sast_to_castexpr e2)
    | Sast.Invoc(recv, fname, args, fuid) -> Cast.Invoc(sast_to_castexpr recv, "f_"^fuid^fname, sast_to_castexprlist args) 
    | _                      -> Cast.Null (* To avoid warning*)


(*Convert the statement list by invoking cstmt on each of the sast stmt*)
let rec cstmtlist slist =  List.map cstmt slist

(*convert sast statement to c statements*)
and cstmt sstmt = 

    let getoptexpr optexpr = 
        match optexpr with
          Some exp -> Some(sast_to_castexpr exp)
        | None     -> None
    in
					
    let rec getiflist iflist =
        match iflist with
	      []                   -> []
		| [(optexpr, slist)]   -> [(getoptexpr optexpr, cstmtlist slist)]
		| (optexpr, slist)::tl -> (getoptexpr optexpr, cstmtlist slist):: getiflist tl    
	in

    match sstmt with
      Sast.Decl(var_def, optexpr, env) -> Cast.Decl(var_def, getoptexpr optexpr, env)
    | Sast.If(iflist, env)             -> Cast.If(getiflist iflist, env)
    | Sast.While(expr, sstmtlist, env) -> Cast.While(sast_to_castexpr expr, cstmtlist sstmtlist, env)
    | Sast.Expr(exp, env)              -> Cast.Expr(sast_to_castexpr exp, env)
    | Sast.Return(optexpr, env)         -> Cast.Return(getoptexpr optexpr, env)
    | Sast.Super(args, fuid, env)       -> (*Cast.Super(sast_to_castexprlist args, env)*)
 					 Cast.Expr(("Void",Cast.Invoc(("This",Cast.This), "f_"^fuid^"_init" ,sast_to_castexprlist args)), env)
  (*  
    | _                                -> raise (Failure "Yet to implement all statement") 
  *)

(**
    Trim up the sast func_def to the cast cfunc_def
    @param func It's a sast func_def. Woo.
    @return It's a cast cfunc_def. Woo.
*)
let sast_to_cast_func (func : Sast.func_def) =
    let (cast_func : Cast.cfunc) =
        {
            returns = func.returns;
            uid = func.uid;
            formals = func.formals;
            static = func.static;
            body = cstmtlist func.body;
            builtin = func.builtin;
        } in
    cast_func

(** Flatten the class
    @param cdef An ast cdef to flatten
    @return A c_struct of a name, a refine set and a variable set
*)
let flatten_cdef (cdef : Ast.class_def) = 
    (** Flatten out our refine list into uids *)
    let flatten_refines refines =
        List.map (fun (x : Ast.func_def) -> x.uid) refines 
    in

    (** Pick out variable members *)
    let rec flatten_section_variables section =
        match section with
        | Ast.VarMem(v)::rest -> v::(flatten_section_variables rest)
        | [] -> []
        | _::rest -> (flatten_section_variables rest) 
    in
    let flatten_variables (sections : Ast.class_sections_def) =
        (flatten_section_variables sections.privates)
        @ (flatten_section_variables sections.protects)
        @ (flatten_section_variables sections.publics)
    in

    let cast_cdef : Cast.class_struct =
        {
            klass     = [cdef.klass];
            refines   = flatten_refines cdef.sections.refines;
            variables = flatten_variables cdef.sections;
        } in
    cast_cdef

(**
    Build a list of the parent chain for a class
    @param klass_data The class data for this program
    @param cdef An ast class_def
    @return A list of the cdefs that comprise the full scope of the original cdef
*)
let rec build_parents klass_data (cdef : Ast.class_def) =
    let parent_cdef =
        StringMap.find ( klass_to_parent cdef ) klass_data.classes
    in
    match cdef with
    | { parent = None } -> [cdef]
    | _ -> cdef :: (build_parents klass_data parent_cdef)

(**
    Pull apart a Ast.class_def
    @param klass_data The class data for this program
    @param sast_cdef A sast class_def
    @return A c_struct and its functions put into a list
*)
let sast_to_cast_cdef klass_data (sast_cdef : Sast.class_def) =
    (** Drop the Sast to an Ast. We don't need environment stuff here *)
    let cdef = StringMap.find sast_cdef.klass klass_data.classes in
    let cdefs = build_parents klass_data cdef in
    (** Function to fold our data again -- I'm making the assumption there are no duplicates *)
    let merge_cdefs (klass_data,merged_cast_cdef) cdef =
        let new_cast_cdef = flatten_cdef cdef in
        let new_merged_cast_cdef = {
            klass = merged_cast_cdef.klass @ new_cast_cdef.klass;
            variables = merged_cast_cdef.variables @ new_cast_cdef.variables;
            refines = merged_cast_cdef.refines @ new_cast_cdef.refines;
        } in
        (klass_data, new_merged_cast_cdef)
    in
    (** Since we're folding, it's easiest to start with an "empty" *)
    let start_cdef = {
        klass = [];
        variables = [];
        refines = [];
    } in
    let cast_cdef = List.fold_left merge_cdefs (klass_data, start_cdef) cdefs in
    (** Pick out variable members *)
    let rec flatten_section_methods section =
        match section with
        | Sast.MethodMem(v)::rest -> (sast_to_cast_func v)::(flatten_section_methods rest)
        | Sast.InitMem(v)::rest -> (sast_to_cast_func v)::(flatten_section_methods rest)
        | [] -> []
        | _::rest -> (flatten_section_methods rest) 
    in
    let flatten_func_section section =
        List.map sast_to_cast_func section
    in
    let flatten_methods (sections : Sast.class_sections_def) =
        (flatten_section_methods sections.privates)
        @ (flatten_section_methods sections.protects)
        @ (flatten_section_methods sections.publics)
        @ (flatten_func_section sections.refines)
        @ (flatten_func_section sections.mains)
    in
    (** It's important to note we really only care about methods of the
    immediate sast. The parent methods will come out of the parent processing
    and the refinements are attached in the struct generation. *)
    let cast_cfunc = flatten_methods sast_cdef.sections in
    (** Simply pop out the uids. *)
    let cast_mains =
        List.map (fun (x : Sast.func_def) -> x.uid) sast_cdef.sections.mains
    in
    (cast_cdef, cast_cfunc, cast_mains)
