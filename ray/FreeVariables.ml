open Parser
open Ast

let _id  = function
	| None -> []
	| Some(x) -> [x]

let rec get_vars_expr = function
	| Id(id) -> [id]
	| This -> []
	|	Null -> []
	| NewObj(the_type, args) -> List.concat (List.map get_vars_expr args) 
	| Anonymous(the_type, args, body) -> (List.concat (List.map get_vars_expr args)) @ (List.concat (List.map get_vars_func_def body))
	| Literal(l) -> []
	| Invoc(receiver, meth, args) -> (get_vars_expr receiver) @ (List.concat (List.map get_vars_expr args))
	| Field(receiver, field) -> (get_vars_expr receiver) @ [field]
	| Deref(var, index) -> get_vars_expr var
	| Unop(an_op, exp) -> get_vars_expr exp
	| Binop(left, an_op, right) -> (get_vars_expr left) @ (get_vars_expr right)
	| Refine(fname, args, totype) -> List.concat (List.map get_vars_expr args)
	|	Assign(the_var, the_expr) -> (get_vars_expr the_var) @ (get_vars_expr the_expr)
	|	Refinable(the_var) -> [the_var]
and apply_opt f = function
	| None -> f Null
	| Some(v) -> f v
and get_vars_var_def (the_type, the_var) = [the_var]
and get_vars_stmt = function
	|	Decl(the_def, the_expr) -> (get_vars_var_def the_def) @ (apply_opt get_vars_expr the_expr)
	| If(clauses) -> List.concat (List.map apply_clause clauses)
	| While(pred, body) -> (get_vars_expr pred) (*@ (List.concat (List.map get_vars_stmt body))*) 
	| Expr(the_expr) -> get_vars_expr the_expr
	| Return(the_expr) -> apply_opt get_vars_expr the_expr
	| Super(args) -> List.concat (List.map get_vars_expr args)
and apply_clause (opt_expr, body) = (apply_opt get_vars_expr opt_expr) (*@ (List.concat (List.map get_vars_stmt body))*)
and get_vars_func_def func = List.concat [(_id func.returns);(List.concat (List.map get_vars_var_def func.formals));(List.concat(List.map get_vars_stmt func.body))]

module StringSet = Set.Make(String)

let free_vars stmts prebound =
  let free_in_expr bound expression = 
    let referenced_vars = get_vars_expr expression in
    let filter fv x = if StringSet.mem x bound then fv  else StringSet.add x fv in
    List.fold_left filter StringSet.empty referenced_vars in

  let free_in_exprs exprlist bound = 
    let var_list = (List.map (free_in_expr bound) exprlist) in
    List.fold_left StringSet.union StringSet.empty var_list in

  let update_stmt = function
    | Decl((var, e))  -> ((_id e), [], Some(var))
    | Expr(e)         -> ([e], [], None)
    | Return(e)       -> ((_id e), [], None)
    | Super(es)       -> (es, [], None)
    | While(e, stmts) -> ([e], [stmts], None)
    | If(parts)       -> let (es, ts) = List.split parts in
                         (Util.filter_option es, ts, None) in

  let rec get_free_vars free bound stmts todo = match stmts, todo with
    | [], [] -> free
    | [], (bound, stmts)::rest -> get_free_vars free bound stmts rest
    | stmt::rest, _ ->
      let (exprs, tasks, decl) = update_stmt stmt in
      let free = StringSet.union free (free_in_exprs exprs bound) in
      let todo = (List.map (function t -> (bound, t)) tasks) @ todo in
      let bound = match decl with
        | Some(var) -> StringSet.add (List.hd (get_vars_var_def var)) bound 
        | _ -> bound in
      get_free_vars free bound rest todo in

  get_free_vars StringSet.empty prebound stmts []
