open Ast
open FreeVariables

let rec list_to_ss = function
	| [] -> StringSet.empty
	| [x] -> StringSet.add x StringSet.empty 
	| hd::tl -> StringSet.add hd (list_to_ss tl)

let _ =  
	let func = List.hd (Debug.get_example_longest_body "Multi" "Collection") in
	let stmts = func.body in
	let prebound = let var_list = List.concat (List.map get_vars_var_def func.formals) in
		list_to_ss var_list in		
	let free_variables = free_vars stmts prebound in
	StringSet.iter (Printf.printf "%s\n") free_variables
