open Sast
open StringModules

let env = StringMap.empty


let get_type exp =
	match exp with
		Literal(l) ->
			match l with ->
				Int(_) -> Int
			|	Float(_) -> Float
			|	String(_) -> String
			|	Bool(_) -> Boolean
				
	|       Binop(e1,op,e2) ->
				let t1 = get_type e1 and
				let t2 = get_type e2
				in
				if(t1 = Int and t2 = Int) then
					Int
				else
					raise "Required Int"
	|       Refinable(s1) -> Boolean
	|       Unop(op, expr) ->
			if get_type expr = Int
				Int
			else
				raise "Required Int for unary"
	
	| 	Invoc(e1, s1, argsexpr_list) ->
		
			List.map get_type argsexpr_list
	
			match get_type e1 with
			| User ->
				method_lookup e1 s1 argsexpr_list
			| _ ->
				raise "Unknown invocation"
		
	|       Assign(e1, e2) ->
				
			let t1 = get_type e1 and t2 = get_type e2
			in
			if t1 = t2 then t1
			else 
				raise "Types do not match"
			
				



let rec attach_bindings stmts env =

    let annotate_expr expr =  (expr , get_type expr)	
    in

    let annotate_exprlist elist = List.map annotate_expr elist
    in

    let build_ifstmt iflist env=
	
	let build_block env (expr, slist) = (annotate_expr expr, (attach_bindings slist env))
	in
	Sast.If( List.map (build_block env) iflist, env)
    in

    let build_env (output, env) stmt =

	match stmt with
		| Ast.While(expr, slist)     	->  (Sast.While((annotate_expr expr, attach_bindings slist env), env)::output, env)
 		| Ast.If (iflist)            	->  ((build_ifstmt iflist env)::output, env)
		| Ast.Decl((vtype,vname),opt_expr)->  (Sast.Decl((vtype, vname), annotate_optexpr opt_expr, env)::output, 
								(StringMap.add vname (vtype,Local) env))
  		| Ast.Expr(expr) 		-> (Sast.Expr(annotate_expr expr, env)::output, env)
		| Ast.Return(expr) 		-> (Sast.Return(annotate_expr expr, env)::output, env)
	        | Ast.Super(expr_list) 		-> (Sast.Super(annotate_exprlist expr_list,env)::output, env)

    in (List.rev (fst(List.fold_left build_env ([],env) stmts)))

