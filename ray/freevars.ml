open Ast
open FreeVariables
open StringModules

let rec get_vars_formals = function
  | [] -> StringSet.empty
  | [(_,var)] -> StringSet.singleton var
  | (_,var)::tl -> StringSet.add var (get_vars_formals tl)

let _ =
  let func = List.hd (Debug.get_example_longest_body "Multi" "Collection") in
  let stmts = func.body in
  let prebound = get_vars_formals func.formals in
  let free_variables = free_vars prebound stmts in
  StringSet.iter (Printf.printf "%s\n") free_variables
