open Ast
module StringMap = Map.Make (String)

(* Types *)
type access_mode = Public | Protected | Private
type method_access_mode = Public | Protected | Private | Refines | Mains
type ('a, 'b) either = Left of 'a | Right of 'b

(* Generic helper functions *)

(* Reduce a list of options to the values in the Some constructors *)
let filter_option list =
  let rec do_filter rlist = function
    | [] -> List.rev rlist
    | None::tl -> do_filter rlist tl
    | (Some(v))::tl -> do_filter (v::rlist) tl in
  do_filter [] list


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
  if StringMap.mem key map
    then StringMap.add key (value::(StringMap.find key map)) map
    else StringMap.add key [value] map

(* Update a map but keep track of collisions *)
let add_map_unique key value (map, collisions) =
  if StringMap.mem key map
    then (map, key::collisions)
    else (StringMap.add key value map, collisions)

(* From a class get the parent *)
let klass_to_parent = function
  | { parent = None; _ } -> "Object"
  | { parent = Some(aklass); _ } -> aklass

(* From a class get the sections of that class *)
let klass_to_sections aklass =
  let s = aklass.sections in [(Public, s.publics); (Protected, s.protects); (Private, s.privates)]

(* Go from a class to all of its sections *)
let klass_to_methods aklass =
  let to_function = function
    | VarMem(_) -> None
    | MethodMem(m) -> Some(m)
    | InitMem(i) -> Some(i) in
  let funcs members = filter_option (List.map to_function members) in
  let s = aklass.sections in
  [(Public, funcs s.publics); (Protected, funcs s.protects); (Private, funcs s.privates); (Refines, s.refines); (Mains, s.mains)]

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

(* Validate that the klass map represents a tree -- everything should go
 * back to object eventually. Returns string option reporting an issue when
 * there is an issue to report.
 *)
let is_tree_hierarchy parent_map =
  let rec from_object klass checked =
    match map_lookup klass checked with
      | Some(true) -> Left(checked)
      | Some(false) -> Right("Cycle detected.")
      | _ -> match map_lookup klass parent_map with
        | None -> Right("Cannot find parent after building parent map: " ^ klass)
        | Some(parent) -> match from_object parent (StringMap.add klass false checked) with
          | Left(updated) -> Left(StringMap.add klass true updated)
          | issue -> issue in
  let folder klass _ = function
    | Left(checked) -> from_object klass checked
    | issue -> issue in
  let checked = StringMap.add "Object" true StringMap.empty in
  match StringMap.fold folder parent_map (Left(checked)) with
    | Right(issue) -> Some(issue)
    | _ -> None

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

(* Checks if two variables are of same type *)
let same_type typ1 typ2 = (typ1 = typ2)

(*Checks if the formal arguments are ambiguous.
 *Pass the appropriate check_ambiguous function.
 *)	
let rec match_formals list1 list2 =
  try List.for_all2 same_type list1 list2 with
    | InvalidArgument -> false

let rec match_args arglist fdeflist =  match fdeflist with 
  | [] ->  None
  | (access,fn)::tl ->
    if (match_formals arglist fn.formals)
      then Some(access, fn)
      else match_args arglist tl


(** we have a classname -> subclassname map check if typ1 is a subtype of typ2
i.e typ1 is same as typ2 or typ1 is same as typ2's child and so on
                or typ2 is typ1's parent
  If the typ1 is a subtyp of typ2, we return a pair (true, level)
  else we return (false, -1)
  If typ1 and typ2 are both same level will be 0
  else it will be the number of levels by which they are apart
**)	

let rec isSubtype typ1 typ2 level sub_to_super_map  =
  if typ1 = typ2  then level
    else if typ1 = "Object" then -1
    else if(StringMap.mem typ1 sub_to_super_map) then
      isSubtype (StringMap.find typ1 sub_to_super_map) typ2 (level + 1) sub_to_super_map
    else -1
		
		

(* Given two argument list, score them on how closely they match with each other
   The first list is the actual arguments, the second list is the formal arguments
 *)
let rec getscore list1 list2 map = match list1, list2 with
  | [], [] ->  [Some(0)]
  | (hd,_)::tl, (x,_)::y -> Some(isSubtype hd x 0 map)::(getscore tl y map)
  | _, _ -> [Some(-1)]
 

(*Given a list of samelength fdefs check if they are compatible
 *Add the fdefs with the compatibility score and build the list.
 *Later we ll ignore fdefs with poor compatibility score.
 *)

let rec get_compatible_fdefs fdef fdeflist map =
  let rec valid alist = match alist with
    | [] -> true
    | [Some x] -> x >= 0
    | hd::tl -> (match hd with
      | Some x -> (x >= 0) && (valid tl)
      | None -> valid tl) in
  match fdeflist with
    | [] -> [None]
    | (access,fn)::tl ->
      let score = getscore fdef.formals fn.formals map in
      if List.length fdef.formals <> List.length fn.formals
        then get_compatible_fdefs fdef tl map
        else if valid score
          then Some(score,access,fn) :: get_compatible_fdefs fdef tl map
          else get_compatible_fdefs fdef tl map

let get_method fdef fdeflist map filtermap =
  let fdef_of (_,x,y) = (x,y) in
  let score_of (x,_,_) i = x in
  let compatiblelist = get_compatible_fdefs fdef fdeflist map in
    match compatiblelist with
      | [] -> None
      | hd::tl -> None (* (score_of hd current),(fdef_of hd)  *)

(* Builds a map of all the methods within a class
 * Key = function name
 * value = list of accessmethod,fdef pairs

 * match_args checks if there is an fdef already in the map,
 * irrespective of its accessmethod, which matches the given
 * method of the class. If no collisions, add the (function def,
 * accessmethod) pair to the list of fdef with the same function
 * name or make a new entry. If the fdef is ambiguous, add the 
 * fdef to the collision list
 *)
 
let build_method_map aklass =
  let add_method access (map,collisions) fdef =
    let found = function
      | Some _ -> true
      | None   -> false in
    let get_match_args = match_args fdef.formals (StringMap.find fdef.name map) in
    if ((StringMap.mem fdef.name map) && (found get_match_args)) then
      then (map, (access,fdef)::collisions)
      else ((add_map_list fdef.name (access,fdef) map), collisions) in
   let map_builder map_pair (access, fdeflist) = List.fold_left (add_method access) map_pair fdeflist in
   build_map_track_errors map_builder (klass_to_methods aklass)

(* Builds a map from classname to map of list of function definition, accessmode pair
 * Key = classname, value = method_map ( from build_method_map function) , 
 * destination map = klass_map.
 * If the method_map construction found collisions, i.e there were function
 * definitions with same name and exactly same argument types, then this method
 * will return the collision list of pairs of (classname , list colliding function def 
 * and access mode pair)
 *)
let build_class_method_map klasses = 
  let map_builder (klass_map, collision_list) aklass =
    match(build_method_map aklass) with
      | Left(method_map) -> (StringMap.add (aklass.klass) method_map klass_map, collision_list)
      | Right(collisions) -> (klass_map, (aklass, collisions)::collision_list) in
  build_map_track_errors map_builder klasses


(* Given a class -> var map table as above, do a lookup -- returns option *)
let class_var_lookup map klassName varName =
  match (map_lookup klassName map) with
    | Some(varTable) -> map_lookup varName varTable
    | None -> None

(*Looks up list of function definitions matching the given function name*)
(*
let method_lookup funcName argList methodmap =
	if (StringMap.mem funcName methodmap)  then
		match_args similar_type argList (StringMap.find funcName methodmap)
	else
		(false, None)

(*Given a class -> method map table , do a fn lookup -- returns option *)
let class_method_lookup map className funcName argtype_list =
  match (map_lookup className map) with 
	|Some(method_map) -> method_lookup funcName argtype_list method_map
	|None  -> None
*)
