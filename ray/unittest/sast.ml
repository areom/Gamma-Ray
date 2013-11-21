module StringMap = Map.Make (String);;

type class_def = { klass : string; parent : string option};;

let d1 = { klass = "myname"; parent = Some("Object") };;
let d3 = { klass = "myname2"; parent = Some("Object1") };;
let d4 = { klass = "myname3"; parent = Some("Object2") };;
let d2 = { klass = "myname1"; parent = Some("Object") };;
let d5= { klass = "Object"; parent = None};;
(*
let myfunc cnameMap cdef = 
   	if StringMap.mem cdef.parent cnameMap then
		let cur = StringMap.find cdef.parent cnameMap in
		StringMap.add cdef.parent (cdef.klass::cur) cnameMap
   	else 
        	StringMap.add cdef.parent [cdef.klass] cnameMap;;

*)
let rec print_list = function 
[] -> print_string "No more subclasses\n";
| e::l -> print_string e ; print_string "," ; print_list l;;

let rec spitmap fst scnd = print_string fst; print_string "->"; print_list scnd;;

let cnameMap = 

let myfunc cnameMap cdef = 

	let cnameMap = StringMap.add cdef.klass [] cnameMap
	in
	let myparent =
		match cdef.parent with
		None -> "Object"
		| Some str -> str 
	in
   	if StringMap.mem myparent cnameMap then
		let cur = StringMap.find myparent cnameMap in
		StringMap.add myparent (cdef.klass::cur) cnameMap
   	else 
        	StringMap.add myparent [cdef.klass] cnameMap;


in
   List.fold_left myfunc StringMap.empty [d1;d2;d3;d4];;
StringMap.iter spitmap cnameMap;;

let s2bmap = 

	let subtobase s2bmap cdef = 
		if StringMap.mem cdef.klass s2bmap then
		         (*how to raise exception*)
			s2bmap	
		else
			StringMap.add cdef.klass cdef.parent s2bmap

	in	
	List.fold_left
		subtobase
	   StringMap.empty [d1;d2;d3;d4;d5];;

let rec spitmap fst snd = print_string fst; print_string "->";
		match snd with
	  	  Some str ->  print_string str;print_string "\n"
		| None -> print_string "Object's parent is none\n";
in
StringMap.iter spitmap s2bmap;;

print_newline
