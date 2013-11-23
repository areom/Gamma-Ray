open Ast
module StringMap = Map.Make (String)

(* Types *)
type access_mode = Public | Protected | Private
type method_access_mode = Public | Protected | Private | Refines | Mains
type ('a, 'b) either = Left of 'a | Right of 'b

(* Builder should accept a StringMap, errors list pair and either return an updated
 * map or an updated error list in the new pair (but hopefully not both). The list
 * is the list of data to build the map out of. So, to put it into types, we have
 *   builder : (StringMap, errorList) -> (StringMap', errorList')
 *)
let build_map_track_errors builder alist =
  match (List.fold_left builder (StringMap.empty, []) alist) with
    | (value, []) -> Left(value)
    | (_, errors) -> Right(errors)

(* Look a value up in a map -- if it is there, return Some(value) else None *)
let map_lookup key map = if StringMap.mem key map then Some(StringMap.find key map) else None

(* Updating a string map that has list of possible values *)
let add_map_list key value map =
  if StringMap.mem key map then StringMap.add key (value::(StringMap.find key map)) map
  else StringMap.add key [value] map

(* Update a map but keep track of collisions *)
let add_map_unique key value (map, collisions) =
  if StringMap.mem key map then (map, key::collisions) else (StringMap.add key value map, collisions)

(* Class inspection functions *)
let klass_to_parent = function
  | { parent = None; _ } -> "Object"
  | { parent = Some(aklass); _ } -> aklass

let klass_to_sections aklass =
  let s = aklass.sections in [(Public, s.publics); (Protected, s.protects); (Private, s.privates)]


let klass_to_methods aklass =
    let rec build_mfuncdef memberdeflist = 
           match memberdeflist with
	   | [] -> [] 
           | h::t -> match h with 
                       MethodMem(h) -> h::build_mfuncdef t
                      |InitMem(h)   -> h::build_mfuncdef t
                      |VarMem(h)     -> build_mfuncdef t
    in
    let s = aklass.sections in
	[(Public,(build_mfuncdef s.publics)); (Protected, (build_mfuncdef s.protects)); (Private, (build_mfuncdef s.privates));
          (Refines, s.refines); (Mains,  s.mains)]

(* make a map children map
 *   parent (name -- string) -> children (names -- string) list
 *)
let build_children_map klasses =
  let map_builder map aklass = add_map_list (klass_to_parent aklass) (aklass.klass) map in
  List.fold_left map_builder StringMap.empty klasses

(* make a map of subclasses to superclasses
 *   class (string) -> parent (string)
 *)
let build_parent_map klasses =
  let map_builder map aklass =
    let parent = klass_to_parent aklass in
    let child  = aklass.klass in
    StringMap.add parent child map in
  List.fold_left map_builder StringMap.empty klasses

(* Build class name -> class def map *)
let build_class_map klasses =
  let map_builder map aklass = StringMap.add (aklass.klass) aklass map in
  List.fold_left map_builder StringMap.empty klasses

(* For a given class, build a map of variable names to variable information.
 * If all instance variables are uniquely named, returns Left (map) where map
 * is  var name -> (access mode, type)  otherwise returns Right (collisions)
 * where collisions are the names of variables that are multiply declared.
 *)
let build_var_map aklass =
  let add_var access map = function
    | VarMem((typeId, varId)) -> add_map_unique varId (access, typeId) map
    | _ -> map in
  let map_builder map (access, section) = List.fold_left (add_var access) map section in
  build_map_track_errors map_builder (klass_to_sections aklass)


let same_type (typ1,_) (typ2,_) =
	if typ1 = typ2 then true
	else false
	
let rec match_formals  list1 list2 =
	match list1,list2 with
	|[],[] -> true
	|[],_ 
        |_,[] -> false
	|h::t,x::y -> (same_type h x) && (match_formals t y)
	
		
 
let build_method_map aklass =
   let match_args _fdef (_,fdef) = match_formals _fdef.formals fdef.formals
   in
   let add_method access (map,collisions) fdef =
	if(StringMap.mem fdef.name map) then
		if(match_args fdef (StringMap.find fdef.name map)) then
			(map, (access,fdef)::collisions)
		else
			((StringMap.add fdef.name ((access, fdef)::(StringMap.find fdef.name map)) map), collisions)
	else
		((StringMap.add fdef.name [(access, fdef)] map)::collisions)
			
   in	
   let map_builder map_pair (access, fdeflist) = 
	List.fold_left (add_method access) map_pair fdeflist
   in
   build_map_track_errors map_builder (klass_to_methods aklass)



(* Build up a map from class to variable map (i.e. to the above).  Returns
 * Left (map) where map is  class -> (var -> (access mode, type))  if all the
 * instance variables in all the classes are okay. Otherwise returns Right
 * (collisions) where collisions is a list of pairs (class, colliding vars)
 *)
let build_class_var_map klasses =
  let map_builder (klass_map, collision_list) aklass =
    match (build_var_map aklass) with
      | Left(var_map) -> (StringMap.add (aklass.klass) var_map klass_map, collision_list)
      | Right(collisions) -> (klass_map, (aklass, collisions)::collision_list) in
  build_map_track_errors map_builder klasses

(*
let build_class_method_map klasses = 
   let map_builder (klass_map, collision_list) aklass =
	match(build_method_map aklass) with
        | Left(method_map) -> (StringMap. (aklass.klass) method_map klass_map, collision_list)
        | Right(colissions) -> (klass_map, (aklass, collisions)::collision_list) in
	build_map_track_errors map_builder klasses
*)

(* Given a class -> var map table as above, do a lookup -- returns option *)
let class_var_lookup map klassName varName =
  match (map_lookup klassName map) with
    | Some(varTable) -> map_lookup varName varTable
    | None -> None

(*
let class_method_lookup map className funcName argtype_list =
  match (map_lookup className map) with 
	|Some

*)
