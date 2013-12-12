
let c_expr_detail sastexp =
	
	match sastexp with
	This ->
	| Null ->
	| Id   ->
	| Literal ->
	| Assign ->
	| Unop  ->
	| Binop ->

let sast_to_castexpr sast_expr = 

	(fst sast_expr, c_expr_detail (snd sast_expr) )
