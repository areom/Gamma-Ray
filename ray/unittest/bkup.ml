module StringMap = Map.Make (String);;

type class_def = { klass : string; parent : string option};;

let d1 = { klass = "myname"; parent = "Object" };;
let d3 = { klass = "myname2"; parent = "Object1" };;
let d4 = { klass = "myname3"; parent = "Object2" };;
let d2 = { klass = "myname1"; parent = "Object" };;

(*let myfunc cnameMap cdef = 
   	if StringMap.mem cdef.parent cnameMap then
		let cur = StringMap.find cdef.parent cnameMap in
		StringMap.add cdef.parent (cdef.klass::cur) cnameMap
   	else 
        	StringMap.add cdef.parent [cdef.klass] cnameMap;;

*)
let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l;;

let rec spitmap fst scnd = print_string fst; print_list scnd;;

let cnameMap = 

let myfunc cnameMap cdef = 
   	if StringMap.mem cdef.parent cnameMap then
		let cur = StringMap.find cdef.parent cnameMap in
		StringMap.add cdef.parent (cdef.klass::cur) cnameMap
   	else 
        	StringMap.add cdef.parent [cdef.klass] cnameMap

in
   List.fold_left
	myfunc
   StringMap.empty [d1;d2;d3;d4];;
StringMap.iter spitmap cnameMap;;

print_newline
