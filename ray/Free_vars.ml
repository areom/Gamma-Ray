open Parser
open Ast

let _id  = function
	| Null -> []
	| x -> [x]

let rec get_vars_expr = function
	| Id(id) -> [id]
	| This -> []
	|	Null -> []
	| NewObj(the_type, args) -> List.concat (List.map get_vars_expr args) 
	| Anonymous(the_type, args, body) -> (List.concat (List.map get_vars_expr args)) (*@ (List.concat (List.map get_vars_func_def body))*)
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
	|	Decl(the_def, the_expr) -> apply_opt get_vars_expr the_expr
	| If(clauses) -> List.concat (List.map apply_clause clauses)
	| While(pred, body) -> (get_vars_expr pred) @ (List.concat (List.map get_vars_stmt body)) 
	| Expr(the_expr) -> get_vars_expr the_expr
	| Return(the_expr) -> apply_opt get_vars_expr the_expr
	| Super(args) -> List.concat (List.map get_vars_expr args)
and apply_clause (opt_expr, body) = (apply_opt get_vars_expr opt_expr) @ (List.concat (List.map get_vars_stmt body))
(*and get_vars_func_def func = List.concat [(apply_opt _id func.returns);(List.concat (List.map get_vars_var_def func.formals));(List.concat(List.map get_vars_stmt func.body))]*)

let rec free_variables = function
	| [] -> []
	| [stmt] -> get_vars_stmt stmt
	| hd::tl -> (get_vars_stmt hd) @ (free_variables tl)
