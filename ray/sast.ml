open Ast

module StringMap = Map.Make (String);;

let getopt value def = 
		match value with
		None -> def
		| Some str -> str

let cnameMap = 
    let classtree cnameMap cdef = 
	let myparent = getopt cdef.parent "None"
	in
   	if StringMap.mem myparent cnameMap then
		let cur = StringMap.find myparent cnameMap 
		in StringMap.add myparent (cdef.klass::cur) cnameMap
   	else 
        	StringMap.add myparent [cdef.klass] cnameMap

   in List.fold_left classtree StringMap.empty program;;
