open Ast

(*
let varray = Array.make 10 0
*)
let rec eval = function
	  Literal(x) ->   x
	| Binop(e1, l, e2) ->
		let getbool(x) = if x = 1 then 1 else 0 in
		let v1 = eval e1 and v2 = eval e2 in
		match l with
			  Add -> v1 + v2
			| Sub -> v1 - v2
			| Prod -> v1 * v2
			| Div -> v1 / v2
			| Mod -> v1 % v2
			| Neg -> -(v1)
			| Pow -> (v1) ^ (v2)
			| Eq ->	getbool(v1=v2)
			| Neq -> getbool(v1!=v2)
			| Less -> getbool(v1<v2)
			| Grtr -> getbool(v1>v2)
			| Leq -> getbool(v1<=v2)
			| Geq -> getbool(v1>=v2)


let _ =

let lexbuf = Lexing.from_channel stdin in

(*let expr = Parser.expr Scanner.token lexbuf in*)

let stmt = Parser.stmt Scanner.token lexbuf in
let result = eval stmt in

print_endline result
