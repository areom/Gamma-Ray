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
let rec getInstanceType vname klass_data kname = 
			match class_var_lookup klass_data kname vname with
			| Some (varmap) ->  Some(varmap, kname)
			| None -> 
					if kname = "Object" then
						None
					else
						let parent = StringMap.find kname klass_data.parents
						in
						getInstanceType vname klass_data parent

(* 
 * Get an Id's type - Not accessed through objects
 * Therefore, it can be a local variable visible in the current scope i.e., present inside env
 * It can be an instance variable in the current class of any access scope
 * Or it can be an instance variable which is either protected or public in any of its ancestor
 * We just get to the closest ancestor
 *)
let getIDType vname env klass_data kname =

		if (StringMap.mem vname env) then 
			fst(StringMap.find vname env)
		else
			let instancedata = getInstanceType vname klass_data kname
			in
			match instancedata with
			Some((section, vtyp), cname) ->
							if kname = cname then
								vtyp
							else if section <> Ast.Privates then
								vtyp
							else
								raise (Failure "ID not in access scope")
			| None -> raise (Failure "Id not found")
			(*Do a lookup on the instance variable for the
			current classdef and return its type else then recurse its ancestor*)
			

let getFieldType recvr member klass_data cur_kname =
	let lookupclass = 
			if recvr = "Kurrent-Klass" then 
				cur_kname
			else
				recvr
	in
	let instancedata = 
				getInstanceType member klass_data lookupclass
	in
	match instancedata with
	Some((section, vtyp), cname) -> if   (cname <> lookupclass && section <> Ast.Publics) then
						raise (Failure "Access only public instance through instance")
					else
						vtyp
	| None -> raise	(Failure "Field unknown")
	
	
let getLiteralType litparam =
		match litparam with
					Ast.Int(i) -> "Integer"
			     	|	Ast.Float(f) -> "Float"
			     	|	Ast.String(s) -> "String"
			     	|	Ast.Bool(b) -> "Boolean"

let getMethodType env klass_data kname methd arglist = 

	match best_method klass_data kname methd arglist with
	 Some(fdef) -> "Some" (*fdef*)
	| None ->    "None"  (* if kname = "Object" then
				raise (Failure "No matching function found")
			else
				let parent = StringMap.find kname klass_data.parents
                      		in
				getMethodType env klass_data kname parent methd arglist
			
*)
let rec eval klass_data kname env exp = 

    	let eval_exprlist env' elist = List.map (eval klass_data kname env') elist
	in
	match exp with
		Ast.This 	-> ("Kurrent-klass", Sast.This)
	|	Ast.Null	-> ("Null", Sast.Null)
	|	Ast.Id(vname)    -> (getIDType vname env klass_data kname, Sast.Id(vname))
	|	Ast.Literal(lit) -> (getLiteralType lit, Sast.Literal(lit))
	|       Ast.NewObj(s1, elist) -> (s1, Sast.NewObj(s1, eval_exprlist env elist))

	| 	Ast.Field(expr, mbr) ->
					let rec recvr = eval klass_data kname env expr
					in
					let recvr_type = fst(recvr)
					in
					(getFieldType recvr_type mbr klass_data kname, Sast.Field(recvr, mbr))

	|       Ast.Invoc(expr, methd, elist) ->
				let recvr = eval klass_data kname env expr
				in
				let recvr_type = fst(recvr)
				in
				let arglist = eval_exprlist env elist 
				in
				let mtype = 
					getMethodType env klass_data kname  methd arglist
				in
				(mtype, Sast.Invoc(recvr, methd, arglist))

	|       Ast.Assign(e1, e2) ->
	
			let t1 = eval klass_data kname env e1  and t2 = eval klass_data kname env e2
			in
			if ((*is_subtype klass_data snd(t2) snd(t1) = *)true) then 
				("Integer", Sast.Assign(t1, t2))
			else 
				raise (Failure "Assigning to incompatible type") 

	|       Ast.Binop(e1,op,e2) ->
				let isCompatible typ1 typ2 = "Integer" (*EDIT*)
				(*	if  Klass.is_subtype gKInfo typ1 typ2 then  typ2
					else if Klass.is_subtype gKinfo typ2 typ1 then  typ1
					else raise (Failure "Binop takes incompatible types")*)
				in
				let t1 = eval klass_data kname env e1 and  t2 = eval klass_data kname env e2
				in	
				let gettype op (_,typ1) (_,typ2) = 
					match op with
						Ast.Arithmetic(_) -> isCompatible typ1 typ2
					|	Ast.NumTest(_)   
					|	Ast.CombTest(_) ->
						  ignore(isCompatible typ1 typ2); "Boolean"
								     
				in (gettype op t1 t2, Sast.Binop(t1,op,t2))
				
(*	| 	Ast.Anonymous(s1, elist, fdef) ->

				 Sast.Anonymous(s1, eval_exprlist env elist,
				(*we have to attach bindings on fdef*)				fdef ), s1 
*)
	|       Ast.Refine(s1, elist, soption) ->
						let arglist = eval_exprlist env elist
						in
						let refinedtype = 
							match soption with
							Some (typ) -> typ
						|	None       -> "None" (*getMethodType env klass_data kname s1 arglist*)
						in
						(refinedtype, Sast.Refine(s1, arglist, soption))
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
					("Integer", Sast.Deref(t1, t2))
					
	|       Ast.Refinable(s1) -> ("Boolean", Sast.Refinable(s1)) (*Check if the method is refinable ?*)

	|       Ast.Unop(op, expr) ->
				let t1 = eval klass_data kname env expr in
		
				("Boolean", Sast.Unop(op,t1))
	|       _  -> "Dummy",Sast.Null
				



(*
 * attach_bindings : Build the Sast, by annotating the expressions with type and statements with env
 * klass_data : global class data record -> type: class_data
 * kname : class name -> type: string
 * stmts : list of Ast statements inside a member function of kname -> type: Ast.stmt list
 * env : map of var declarations visible in the current scope   - > type: environment 
*)
let rec attach_bindings klass_data kname stmts env =


    let eval_exprlist env' elist = List.map (eval klass_data kname env') elist
    in

    (*	build_ifstmt iflist env ->
	iflist: Ast.if
	env : enviroment in its scope 
	Builds a Sast If, env -> evaluates expressions and annotates type 
				binds env to the statement list
    *)
    let build_ifstmt iflist env=
	
	let build_block env (exp, slist) =
	
	     let exprtyp = 

		match exp with
		  None -> None
		| Some exp ->
				let checktype (typ, exp) =
					if typ = "Boolean" then
						(typ, exp)
					else
						raise (Failure "Predicates must be boolean")
				in
				Some(checktype (eval klass_data kname env exp))

	     in
	     (exprtyp, attach_bindings klass_data kname slist env)
	in
	Sast.If( List.map (build_block env) iflist, env)
    in 

(*
 * Build the environment (actually Sast) for every Ast statement, 
 * build the corressponding Sast.ssmt which is Ast.stmt * env
 * while updating the env in its scope if there was a new declaration
 *  and for every Ast.expr, annotate it with type -> type, Ast.expr
*)
    let build_env (output, env) stmt =

	match stmt with
		| Ast.While(expr, slist) -> 
						let exprtyp = 
							let e1 = eval klass_data kname env expr
							in
								match fst(e1) with
									"Boolean" -> e1
									| _  -> raise (Failure "While expects Boolean")
						in		
						(Sast.While(((exprtyp), attach_bindings klass_data kname slist env), env)::output, env)


 		| Ast.If (iflist)      	->  	((build_ifstmt iflist env)::output, env) 

		| Ast.Decl((vtype,vname),opt_expr) ->
							let sastexpr = 
								match opt_expr with 
								Some exp -> Some(eval klass_data kname env exp)
								| None -> None
						 	
							in
						 	(Sast.Decl((vtype, vname), sastexpr , env)::output, 
								(StringMap.add vname (vtype,Local) env))

  		| Ast.Expr(expr) 	-> 	(Sast.Expr((eval klass_data kname env expr), env)::output, env)
		| Ast.Return(opt_expr) 	->
						let sastexpr = 
							match opt_expr with
							   	  Some exp -> Some (eval klass_data kname env exp)
								| None -> None
						 in
						 (Sast.Return(sastexpr, env)::output, env)

	        | Ast.Super(expr_list) 	-> (Sast.Super(eval_exprlist env expr_list,env)::output, env)

    in (List.rev (fst(List.fold_left build_env ([],env) stmts)))

