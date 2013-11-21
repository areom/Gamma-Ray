module StringMap = Map.Make (String);;

type class_def = { klass : string; parent : string option};;

let d1 = { klass = "myname"; parent = Some("Object") };;
let d3 = { klass = "myname2"; parent = Some("Object1") };;
let d4 = { klass = "myname3"; parent = Some("Object2") };;
let d2 = { klass = "myname1"; parent = Some("Object") };;
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
	   StringMap.empty [d1;d2;d3;d4];;

let rec spitmap fst snd = print_string fst; print_string "->";
		match snd with
	  	  Some str ->  print_string str;print_string "\n"
		| None -> print_string "Object's parent is none\n";
in
StringMap.iter spitmap s2bmap;;

print_newline;;


print_string "getclassdef test\n\n";;
let rec getclassdef cname clist = 
	match clist with
	[] -> None
	| hd::tl -> if hd.klass = cname then Some(hd) else getclassdef cname tl;;

let print_cdef c = match c with None -> "No classdef" | Some c1 -> c1.klass;;
let print_pdef p = match p with None -> "No classdef" | Some p1 ->
			(match p1.parent with None -> "No parent" | Some x -> x);;

let def1 = getclassdef "myname" [d1;d2;d3;d4];;
print_string (print_cdef def1);; 
print_string "\n";;
print_string(print_pdef def1);;

print_string "\n\ngetmethoddef test\n";;


type var_def = string * string;;
type func_def = {
  returns : string option;
  host    : string option;
  name    : string;
  static  : bool;
  formals : var_def list;
  (*body    : stmt list;*)
};;
type member_def = VarMem of var_def | MethodMem of func_def | InitMem of func_def;;

(* Things that can go in a class *)
type class_sections_def = {
  privates : member_def list;
  protects : member_def list;
  publics  : member_def list;
  refines  : func_def list;
  mains    : func_def list;
};;
	

type methodenv = {

	cname : string option; (*class it belongs to*)
	access: string option;(*access specifier*)
	astrec: func_def option; (*ast record*)

};;

let rec getmemdef mname mlist = 
	match mlist with
	[] -> None
	| hd::tl -> if hd.(*hd = member_def - > we need the var_def's first*) = mname then Some(hd) else getclassdef mname tl;;

(*
let lookupfield cdef vname = 
    let pmem = getmemdef vname cdef.class_sections_def.privates;
    match pmem with 
	Some def -> (cdef.klass, "private", def)
     |  None     -> 
		let pubmem = getmemdef vname cdef.class_sections_def.publics;
		match pubmem with 
			Some def -> (cdef.klass, "public", def)
		     |  None     ->  
				let promem = getmemdef vname cdef.class_sections_def.protects;
				match promem with 
					Some def -> (cdef.klass, "protect", def)
				   |    None  -> None


let getfield cname vname =
	let classdef = getclassdef cname
	match classdef with 
             None -> None
	|    Some (cdef) -> lookup_field cdef.class_sections_def vname


*)	
