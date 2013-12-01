open Ast
open FreeVariables

let _ =  
	let func = List.hd (Debug.get_example_longest_body "Multi" "FreeVarTest") in
	let stmts = func.body in
	let free_variables = free_vars stmts StringSet.empty in
	StringSet.iter (Printf.printf "%s\n") free_variables
