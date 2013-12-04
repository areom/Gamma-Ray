open Sast
(*open StringModules*)
module StringMap = Map.Make(String)

let env = StringMap.empty

(*
Ï Used identiﬁers must be deﬁned

Ï Function calls must refer to functions

Ï Identiﬁer references must be to variables

Ï The types of operands for unary and binary operators must be

consistent.

Ï The predicate of an if and while must be a Boolean.

Ï It must be possible to assign the type on the right side of an

assignment to the lvalue on the left
*)
let rec eval env exp = 

	match exp with
		Ast.This 	-> Sast.Null, Boolean
	|	Ast.Null	-> Sast.Null, User("Null")
	|	Ast.Id(name)    ->
			   		if (StringMap.mem name env) then 
						
						let vartyp = StringMap.find name env

						in
						(Sast.Id(name),  fst(vartyp))
					else
						raise (Failure "Undefined Identified")

	|	Ast.Literal(lit) -> 
			let get_typ t1= 
				match t1 with 
					Ast.Int(i) -> Integer
			     	|	Ast.Float(f) -> FloatPt
			     	|	Ast.String(s) -> Strings
			     	|	Ast.Bool(b) -> Boolean
			in
			(Sast.Literal(lit), get_typ lit)

	|       Ast.Binop(e1,op,e2) ->
				let t1 = eval env e1 and  t2 = eval env e2
				in
				if(snd(t1) = Integer && snd(t2) = Integer) then
				   let gettype op =
					match op with
						 Ast.Arithmetic(_) -> Integer
				       		|Ast.NumTest(_) -> Boolean
				       		|Ast.CombTest(_) -> Boolean

				   in (Sast.Binop(t1,op,t2), gettype op)
				
				else
					raise(Failure "Bin op needs Integer error")
					
	|       Ast.Refinable(s1) -> (Sast.Refinable(s1), Boolean) (*Check if the method is refinable*)

	|       Ast.Unop(op, expr) ->
				let t1 = eval env expr in
		
				if(snd(t1) = Integer || snd(t1) = Boolean) then
					(Sast.Unop(op,t1), Boolean)
				else
					raise(Failure "Unary op takes Integer error") 
				
(*	
	| 	Invoc(e1, s1, argsexpr_list) ->
		
			List.map get_type argsexpr_list
	
			match get_type e1 with
			| User ->
				method_lookup e1 s1 argsexpr_list
			| _ ->
				Undef
*)
		
	|       Ast.Assign(e1, e2) ->
				
			let t1 = eval env e1  and t2 = eval env e2
			in
			if snd(t1) = snd(t2) then 
				(Sast.Assign(t1, t2), snd(t1))
			else 
				raise (Failure "Assign type failed")
	|        _ -> (Sast.Null, Boolean)
				




let rec attach_bindings stmts env =


    let eval_exprlist env' elist = List.map (eval env') elist
    in

    let build_ifstmt iflist env=
	
	let build_block env (exp, slist) =
	
	     let exprtyp = 

		match exp with
		  None -> None
		| Some exp -> Some(eval env exp)

	     in
	     (exprtyp, attach_bindings slist env)
	in
	Sast.If( List.map (build_block env) iflist, env)
    in 

    let build_env (output, env) stmt =

	match stmt with
		| Ast.While(expr, slist)  -> 
						let exprtyp = 
							let e1 = eval env expr
							in
							match snd(e1) with
								Boolean -> e1
								| _  -> raise (Failure "While expects Boolean")
						in		
						(Sast.While(((exprtyp), attach_bindings slist env), env)::output, env)

 		| Ast.If (iflist)            	->  ((build_ifstmt iflist env)::output, env) 
		| Ast.Decl((vtype,vname),opt_expr)->
							let convert_type stringed =
								match stringed with
								"Integer" -> Integer
							   |    "Boolean" -> Boolean	
							   |    "String"  -> Strings
							   |    "Float"   -> FloatPt
							   |    others    -> User(others)
							in
							
							let exprtyp = 
								match opt_expr with 
								Some exp -> Some(eval env exp)
								| None -> None
						 	in
						 	(Sast.Decl((convert_type vtype, vname), exprtyp , env)::output, 
								(StringMap.add vname (convert_type vtype,Local) env))

  		| Ast.Expr(expr) 		-> (Sast.Expr((eval env expr), env)::output, env)
		| Ast.Return(opt_expr) 		->
						 let exprtyp = 
							match opt_expr with
						   	  Some exp -> Some (eval env exp)
							| None -> None
						 in
						 (Sast.Return(exprtyp, env)::output, env)

	        | Ast.Super(expr_list) 		-> (Sast.Super(eval_exprlist env  expr_list,env)::output, env)

    in (List.rev (fst(List.fold_left build_env ([],env) stmts)))

