open Ast

(*
let varray = Array.make 10 0
*)(*
let rec eval = function
(*	  ILIT(x) ->  print_endline x; x
	| Binop(e1, op, e2) ->
		let v1 = eval e1 and v2 = eval e2 in
		match op with
			  Add -> v1 + v2
			| Sub -> v1 - v2
			| Mul -> v1 * v2
			| Div -> v1 / v2
*)
	While(e1,e2) -> "hello world";	
*)
let rec eval = function
	Block (e1) -> "hello world"
	| _ -> "lost world"
let _ =

let lexbuf = Lexing.from_channel stdin in

(*let expr = Parser.expr Scanner.token lexbuf in*)

let stmt = Parser.stmt Scanner.token lexbuf in
let result = eval stmt in

print_endline result
