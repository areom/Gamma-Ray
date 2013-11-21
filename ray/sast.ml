open Ast

module StringMap = Map.Make (String);;

let getopt value def = 
		match value with
		None -> def
		| Some str -> str

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


let s2bmap = 
	let subtobase s2bmap cdef = 
		if StringMap.mem cdef.klass s2bmap then
		         (*how to raise exception*)
			s2bmap	
		else
			StringMap.add cdef.klass cdef.parent s2bmap
	in	
	List.fold_left subtobase StringMap.empty program;;


let rec getclassdef cname clist = 
	match clist with
	[] -> None
	| hd::tl -> if hd.klass = cname then Some(hd) else getclassdef cname tl;;

