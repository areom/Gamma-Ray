open Ast

module StringMap = Map.Make (String);;

let fstoffour (x,_,_,_) = x;;
let sndoffour (_,x,_,_) = x;;
let throffour (_,_,x,_) = x;;
let lstoffour (_,_,_,x) = x;;


(* build_children_map
   make a map of classname to direct subclassnames
 
   key = classname val = list of subclassnames
   "SuperClass" -> ["A";"B";]
    "A" -> []
    "B" -> []
*)

let klass_to_parent = function
  | { parent = None; _ } -> "Object"
  | { parent = Some(klass); _ } -> klass

let build_children_map klasses =
  let map_builder map aklass =
    let parent = klass_to_parent aklass in
    let child  = aklass.klass in
    if StringMap.mem parent map then
      let children = StringMap.find parent map in
      StringMap.add parent (child::children) map
    else
      StringMap.add parent [child] map in
  List.fold_left map_builder StringMap.empty klasses


(* build_parent_map
   make a map of subclasses to superclasses
   
   key = class      val = parent
   A -> Object
*)

let build_parent_map klasses =
  let map_builder map aklass =
    let parent = klass_to_parent aklass in
    let child  = aklass.klass in
    StringMap.add parent child map in
  List.fold_left map_builder StringMap.empty klasses


(*given class name and class_def list, get the matching class_def*)

let rec getclassdef cname clist = 
	match clist with
	[] -> None
	| hd::tl -> if hd.klass = cname then Some(hd) else getclassdef cname tl;;





(*Given a class definition and variable name, the lookupfield
looksup for the field in the privates, publics and protects list of the class_def sections.
If found returns a (classname, accessspecifier, typeid, variablename) tuple
If not found returns a None*)
let lookupfield cdef vname = 
    let pmem = getmemdef vname cdef.sections.privates 
    in
    match pmem with 
	Some def -> Some(cdef.klass, "private", vname, def)
     |  None     -> 
		let pubmem = getmemdef vname cdef.sections.publics
		in
		match pubmem with 
			Some def -> Some(cdef.klass, "public", vname, def)
		     |  None     ->  
				let promem = getmemdef vname cdef.sections.protects 
				in
				match promem with 
					Some def -> Some(cdef.klass, "protect", vname, def)
				   |    None  -> None
;;

(*getfield takes classname and variablename;
  looks for the class with the classname;
  If classname found, looksup the variable in the class;
  Else returns None
*)

let rec getfield cname vname cdeflist =
	let classdef = getclassdef cname cdeflist
	in
	match classdef with 
             None -> 
		if cname = "Object" then
			None
		else
			let basename = match(StringMap.find cname s2bmap) with Some b -> b | None -> "Object"
			in 
			getfield basename vname cdeflist
	|    Some (cdef) -> lookupfield cdef vname;;

(*
USAGE:
let field = getfield "myname" "e" class_def_list
in 
match field with
None -> print_string "field not found\n";
| Some tup -> print_string (fstoffour(tup));;

*)

	
