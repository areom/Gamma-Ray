
let c_expr_detail sastexp =
	
	match sastexp with
	  Sast.This ->             
	| Sast.Null ->
	| Sast.Id(vname)   ->    
	| Sast.Literal(lit) ->
	| Sast.Assign(e1, e2) ->
	| Sast.Unop(op, expr)  ->
	| Sast.Binop(e1, op, e2) ->

let sast_to_castexpr sast_expr = 

	(fst sast_expr, c_expr_detail (snd sast_expr) )
