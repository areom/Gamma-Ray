open Ast

let _ =  
	let func = List.hd (Debug.get_example_longest_body "Multi" "FreeVarTest") in
	let stmts = func.body in
	let free_vars = FreeVariables.free_variables stmts in
	print_string (String.concat "\n" free_vars); print_newline() 
