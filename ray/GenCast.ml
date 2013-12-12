
let rec sast_to_castexpr sast_expr = 

	(fst sast_expr, c_expr_detail (snd sast_expr) )

and c_expr_detail sastexp =
	
	match sastexp with
	  Sast.This ->          Cast.This
	| Sast.Null ->		Cast.Null
	| Sast.Id(vname)   ->    Cast.Id(vname)
	| Sast.Literal(lit) ->	 Cast.Literal(lit)
	| Sast.Assign(e1, e2) ->    Cast.Assign( sast_to_castexpr e1, sast_to_castexpr e2)
	| Sast.Unop(op, expr)  ->   Cast.Unop(op, sast_to_castexpr expr)
	| Sast.Binop(e1, op, e2) ->  Cast.Binop(sast_to_castexpr e1, op, sast_to_castexpr e2)
        | Sast.Deref(e1, e2) ->      Cast.Deref(sast_to_castexpr e1, sast_to_castexpr e2)
	| _ 			->  Cast.Null
