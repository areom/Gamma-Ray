type t= Int | Float | String |Bool 


module StringMap = Map.Make(String)


type 'a lookup_map = 'a StringMap.t

(*type sexpr = expr * t*)
type sstmt = Ast.stmt * ((string * string option)) lookup_map


let env = StringMap.empty

let rec attach_bindings stmts env =

    let first_of (l, _) = l in

    let build_ifstmt iflist env=
	
	let build_block env ifblock = 

		match ifblock with
			(Some expr, slist) -> (Some expr, (attach_bindings slist env))
	    	|   	(None,  slist)     -> (None, (attach_bindings slist env))

	in
	(Ast.If(List.map (build_block env) iflist), env)
		
    in

    let build_env (output, env) stmt =

	match stmt with
		| Ast.While(expr, slist)     ->  ((Ast.While(expr, attach_bindings slist env), env)::output, env)
 		| Ast.If (iflist)            ->  ((build_ifstmt iflist env)::output, env)
		| Ast.Decl((vname,vtype), _) ->  ((stmt, env)::output, (StringMap.add vname(vtype,None) env))
		| _  -> ((stmt,env)::output, env)

    in List.rev (first_of(List.fold_left build_env ([],env) stmts))
