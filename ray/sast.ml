open Ast

module StringMap = Map.Make (String);;

let getopt value def = 
		match value with
		None -> def
		| Some str -> str


let fstoffour (x,_,_,_) = x;;
let sndoffour (_,x,_,_) = x;;
let throffour (_,_,x,_) = x;;
let lstoffour (_,_,_,x) = x;;


(* base2subMap is a map of classname to subclassnames
   Add classname as key with empty list;
   Add classname to parent's list;
 
   key = classname      val = list of subclassnames
   "SuperClass" -> ["A";"B";]
    "A" -> []
    "B" -> []
*)
let base2subMap = 

    let buildBase2Sub base2subMap cdef = 

	let base2subMap = StringMap.add cdef.klass [] base2subMap 
	in
	let myparent = getopt cdef.parent "Object"
	in
   	if StringMap.mem myparent base2subMap then
		let cur = StringMap.find myparent base2subMap 
		in StringMap.add myparent (cdef.klass::cur) base2subMap
   	else 
        	StringMap.add myparent [cdef.klass] base2subMap

   in List.fold_left buildBase2Sub StringMap.empty program;;


(* s2bMap is a map of class names to immediate parent
   by default all classes extend Object class - Ast
   has this already set it so we simply create a mapping.
   
   key = class      val = parent
   A -> Object
*)
let s2bmap = 
	let subtobase s2bmap cdef = 
		if StringMap.mem cdef.klass s2bmap then
		         (*how to raise exception*)
			s2bmap	
		else
			StringMap.add cdef.klass cdef.parent s2bmap
	in	
	List.fold_left subtobase StringMap.empty program;;


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

	
