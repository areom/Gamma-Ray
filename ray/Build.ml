open Sast
module StringMap = Map.Make(String)
let env = StringMap.empty

let rec attach_bindings stmts env =

    let build_ifstmt iflist env=
	
	let build_block env (condn, slist) = (condn, (attach_bindings slist env))

	in
	Sast.If( List.map (build_block env) iflist, env)
		
    in

    let build_env (output, env) stmt =

	match stmt with
		| Ast.While(expr, slist)     ->  (Sast.While((expr, attach_bindings slist env), env)::output, env)
 		| Ast.If (iflist)            ->  ((build_ifstmt iflist env)::output, env)
		| Ast.Decl((vtype,vname), opt) ->  (Sast.Decl((vtype, vname), opt, env)::output, (StringMap.add vname (vtype,Local) env))
  		| Ast.Expr(exp) -> (Sast.Expr(exp, env)::output, env)
		| Ast.Return(exp) -> (Sast.Return(exp, env)::output, env)
	        | Ast.Super(elist) -> (Sast.Super(elist,env)::output, env)

    in (List.rev (fst(List.fold_left build_env ([],env) stmts)))
