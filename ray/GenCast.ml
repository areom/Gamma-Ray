open Sast
open Cast

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
    | Sast.Return(optexpr,env)         -> Cast.Return(getoptexpr optexpr, env)
    | Sast.Super(exprlist, env)       -> Cast.Super(sast_to_castexprlist exprlist, env)
    | _                                -> raise (Failure "Yet to implement all statement")

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
        } in
    cast_func
(**
(**
    Pull apart a Sast.class_def
    @param cdef A sast class_def
    @return An ordered pair of the cast class_def and its functions serialized
*)
let sast_to_cast_cdef cdef =
    let flatten_refines refines =
      
    in

    let rec flatten_section_variables section =
        match refines with
        | Sast.VarMem(v)::rest ->  
        | [] 
        | 
    in

    let cast_cdef : Cast.class_struct =
        {
            klass     = cdef.klass;
            refines   = flatten_refines cdef.sections.refines;
            variables = flatten_variables cdef.sections;
        } in
    cast_cdef
*)
