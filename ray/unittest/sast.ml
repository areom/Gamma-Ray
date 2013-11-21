module StringMap = Map.Make (String);;



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
(*  refines  : func_def list;
  mains    : func_def list;*)
};;
	
type class_def = { klass : string; parent : string option; sections : class_sections_def; };;

let sdef1 = { 
 privates = [VarMem("int","a"); VarMem("int","b");];
 protects = [VarMem("int","c"); VarMem("int","d");];
 publics  = [VarMem("int","e"); VarMem("int","f");];
};;

let sdef2 = { 
 privates = [ VarMem("int","g"); VarMem("int","h");];
 protects = [ VarMem("int","j"); VarMem("int","i");];
 publics = [ VarMem("int","k"); VarMem("int","l");];
};;

let sdef3 = { 
 privates = [ VarMem("int","m"); VarMem("int","n");];
 protects = [ VarMem("int","p"); VarMem("int","o");];
 publics = [ VarMem("int","q"); VarMem("int","r");];
};;

let sdef4 = { 
 privates = [VarMem("int","x"); VarMem("int","s");];
 protects = [VarMem("int","w"); VarMem("int","t");];
 publics = [VarMem("int","v"); VarMem("int","u");];
};;
let d1 = { klass = "myname"; parent = Some("Object"); sections = sdef1 };;
let d3 = { klass = "myname2"; parent = Some("myname1");  sections = sdef3; };;
let d4 = { klass = "myname3"; parent = Some("myname2");  sections = sdef4; };;
let d2 = { klass = "myname1"; parent = Some("myname"); sections = sdef2; };;
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



let rec getmemdef mname mlist = 
	match mlist with
	[] -> None
	| hd::tl -> match hd with
			VarMem(typeid, varname) -> if varname = mname then Some(typeid) else getmemdef mname tl
			| _ -> None
;;

(*Given a class definition and variable name, the lookupfield
looksup for the field in the privates, publics and protects list.
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
let fstoffour (x,_,_,_) = x;;
let sndoffour (_,x,_,_) = x;;
let throffour (_,_,x,_) = x;;
let lstoffour (_,_,_,x) = x;;

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

let field = getfield "myname3" "a" [d1;d2;d3;d4]
in 
match field with
None -> print_string "field not found\n";
| Some tup -> print_string (fstoffour(tup));;



	
