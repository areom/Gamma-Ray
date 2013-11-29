open Ast
module StringMap = Map.Make (String)


let attach_bindings env stmts =

     let build_env env stmt =

	let env =
	        (*add the var decl to env*)	
		let update_env (vname,vtyp) env = 
			StringMap.add vname (vtyp, None) env
		in 
		(*Check if the statement is var decl return updated env*)
		match stmt with
		| Decl(var_def,Some(expr)) -> update_env var_def env 
		| _ 	-> env
	in
	(*Build a stmt,env pair*)
	(stmt, env)	
	in
	(*build a list of (stmt,env) pair *)
	List.map (build_env env) stmts
