open Sast
open Klass
open Str
(*open StringModules*)
module StringMap = Map.Make(String)

let env = StringMap.empty

(*ADD MORE CHECKS*)

(*Get the parent of kname
		and invoke getInstanceType on it 
				if kname has no parent 
					throw exception*)
let rec getInstanceType vname env klass_data kname = 
			match class_var_lookup klass_data kname vname with
			| Some (varmap) ->  Some(varmap, kname)
			| None -> 
					if kname = "Object" then
						None
					else
						let parent = StringMap.find kname klass_data.parents
						in
						getInstanceType vname env klass_data parent

let getIDType vname env klass_data kname =

		if (StringMap.mem vname env) then 
			fst(StringMap.find vname env)
		else
			let instancedata = getInstanceType vname env klass_data kname
			in
			match instancedata with
			Some((section, vtyp),cname) ->
							if kname = cname then
								vtyp
							else if section <> Ast.Privates then
								vtyp
							else
								raise (Failure "ID private")
			| None -> raise (Failure "Id not found")
			(*Do a lookup on the instance variable for the
			current classdef and return its type else then recurse its ancestor*)
			

let getFieldType member env klass_data kname =

	let instancedata = getInstanceType member env klass_data kname
	in
	match instancedata with
	Some((section, vtyp), cname) -> if section <> Ast.Publics then
						raise (Failure "Instance can acess only public")
					else
						vtyp
	| None -> raise	(Failure "Field unknown")
	
	
let getLiteralType litparam =
		match litparam with
					Ast.Int(i) -> "Integer"
			     	|	Ast.Float(f) -> "Float"
			     	|	Ast.String(s) -> "String"
			     	|	Ast.Bool(b) -> "Boolean"

let getMethodType env klass_data kname methd arglist = "String"

	(*	let kdef =  klass_lookup kname
		in 
		let mdef =  method_lookup methd arglist
		in
*)
		(*Do a lookup on the classname, 
		  Get the function definitions 
		  and check if a matching function
		  exist and return its type else
		  recurse on its ancestor *)	

let rec eval klass_data kname env exp = 

    	let eval_exprlist env' elist = List.map (eval klass_data kname env') elist
	in
	match exp with
		Ast.This 	-> (Sast.This, "Current-Klass")
	|	Ast.Null	-> (Sast.Null, "Null")
	|	Ast.Id(vname)    -> (Sast.Id(vname),  getIDType vname env klass_data kname)
	|	Ast.Literal(lit) -> (Sast.Literal(lit), getLiteralType lit)
	|       Ast.NewObj(s1, elist) -> (Sast.NewObj(s1, eval_exprlist env elist), s1)

	| 	Ast.Field(expr, mbr) ->
					let rec recvr = eval klass_data kname env expr in
					let recvr_type = snd(recvr) in
					(Sast.Field(recvr, mbr), getFieldType mbr env klass_data recvr_type)	
	|       Ast.Invoc(expr, methd, elist) ->

					let recvr_type param = snd(param) in
					let rec recvr = eval klass_data kname env expr and arglist = eval_exprlist env elist 
					in
					(Sast.Invoc(recvr, methd, arglist), getMethodType env klass_data recvr_type methd arglist)

	|       Ast.Assign(e1, e2) ->
	
			let t1 = eval klass_data kname env e1  and t2 = eval klass_data kname env e2
			in
			if ((*is_subtype klass_data snd(t2) snd(t1) = *)true) then 
				(Sast.Assign(t1, t2), snd(t1))
			else 
				raise (Failure "Assigning to incompatible type") 

	|       Ast.Binop(e1,op,e2) ->
				let isCompatible typ1 typ2 = typ1 (*EDIT*)
				(*	if  Klass.is_subtype gKInfo typ1 typ2 then  typ2
					else if Klass.is_subtype gKinfo typ2 typ1 then  typ1
					else raise (Failure "Binop takes incompatible types")*)
				in
				let t1 = eval klass_data kname env e1 and  t2 = eval klass_data kname env e2
				in	
				let getype op (_,typ1) (_,typ2) = 
					match op with
						Ast.Arithmetic(_) -> isCompatible typ1 typ2
					|	Ast.NumTest(_)   
					|	Ast.CombTest(_) ->
						  ignore(isCompatible typ1 typ2); "Boolean"
								     
				in (Sast.Binop(t1,op,t2),getype op t1 t2)
				
(*	| 	Ast.Anonymous(s1, elist, fdef) ->

				 Sast.Anonymous(s1, eval_exprlist env elist,
				(*we have to attach bindings on fdef*)				fdef ), s1 

	|       Ast.Refine(s1, elist, soption) ->
	|
*)
	|       Ast.Deref(e1, e2) ->
					let expectArray typename = 
						match last_chars typename 2 with
							"[]"    ->  List.hd (split (regexp "\[") typename)
						|	_	-> raise (Failure "Not an array type")
					in
					let t1 = eval klass_data kname env e1 and t2 = eval klass_data kname env e2
					in
					let getArrayType (_, typ1) (_,typ2) = 
						if typ2 = "Integer" then  expectArray typ2 
						else raise(Failure "Dereferencing invalid")
					in
					(Sast.Deref(t1, t2), getArrayType t1 t2)
					
	|       Ast.Refinable(s1) -> (Sast.Refinable(s1), "Boolean") (*Check if the method is refinable ?*)

	|       Ast.Unop(op, expr) ->
				let t1 = eval klass_data kname env expr in
		
				(Sast.Unop(op,t1), "Boolean")
	|       _  -> Sast.Null, "dummy"
				




let rec attach_bindings klass_data kname stmts env =


    let eval_exprlist env' elist = List.map (eval klass_data kname env') elist
    in

    let build_ifstmt iflist env=
	
	let build_block env (exp, slist) =
	
	     let exprtyp = 

		match exp with
		  None -> None
		| Some exp ->
				let checktype (exp, typ) =
					if typ = "Boolean" then
						(exp,typ)
					else
						raise (Failure "Predicates must be boolean")
				in
				let exptype = eval klass_data kname env exp
				in
				Some(checktype exptype)

	     in
	     (exprtyp, attach_bindings klass_data kname slist env)
	in
	Sast.If( List.map (build_block env) iflist, env)
    in 

    let build_env (output, env) stmt =

	match stmt with
		| Ast.While(expr, slist)  -> 
						let exprtyp = 
							let e1 = eval klass_data kname env expr
							in
							match snd(e1) with
								"Boolean" -> e1
								| _  -> raise (Failure "While expects Boolean")
						in		
						(Sast.While(((exprtyp), attach_bindings klass_data kname slist env), env)::output, env)

 		| Ast.If (iflist)            	->  ((build_ifstmt iflist env)::output, env) 
		| Ast.Decl((vtype,vname),opt_expr)->
							let exprtyp = 
								match opt_expr with 
								Some exp -> Some(eval klass_data kname env exp)
								| None -> None
						 	in
						 	(Sast.Decl((vtype, vname), exprtyp , env)::output, 
								(StringMap.add vname (vtype,Local) env))

  		| Ast.Expr(expr) 		-> (Sast.Expr((eval klass_data kname env expr), env)::output, env)
		| Ast.Return(opt_expr) 		->
						 let exprtyp = 
							match opt_expr with
						   	  Some exp -> Some (eval klass_data kname env exp)
							| None -> None
						 in
						 (Sast.Return(exprtyp, env)::output, env)

	        | Ast.Super(expr_list) 		-> (Sast.Super(eval_exprlist env expr_list,env)::output, env)

    in (List.rev (fst(List.fold_left build_env ([],env) stmts)))

