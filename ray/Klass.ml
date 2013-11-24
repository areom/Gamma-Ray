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
let map_lookup key map = if StringMap.mem key map
  then Some(StringMap.find key map)
  else None

(* Look a value list up in a map -- if it is there, return the list else [] *)
let map_lookup_list key map = if StringMap.mem key map
  then StringMap.find key map
  else []

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

(* Given a class -> var map table as above, do a lookup -- returns option *)
let class_var_lookup map klass_name var_name =
  match (map_lookup klass_name map) with
    | Some(var_table) -> map_lookup var_name var_table
    | _ -> None

(* Return whether two function definitions have conflicting signatures *)
let conflicting_signatures func1 func2 =
  let same_type (t1, _) (t2, _) = (t1 = t2) in
  let same_name = (func1.name = func2.name) in
  let same_params = try List.for_all2 same_type func1.formals func2.formals with
    | Invalid_argument(_) -> false in
  same_name && same_params

(* Builds a map of all the methods within a class, returning a list of collisions
 * when there are conflicting signatures. Keys to the map are function names and
 * the values are list of (access mode, func_def) pairs.
 *)
let build_method_map aklass =
  let add_method access (map, collisions) fdef =
    let same_name = List.map (function (_, func) -> func) (map_lookup_list fdef.name map) in
    if List.exists (conflicting_signatures fdef) same_name
      then (map, fdef::collisions)
      else (add_map_list fdef.name (access, fdef) map, collisions) in
  let map_builder map (access, funcs) = List.fold_left (add_method access) map funcs in
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

(* Given a class -> method map table, look up all the methods in a given class with a given name
 * Returns a list of func_defs. If no method can be found, returns the empty list
 *)
let class_method_lookup map klass_name func_name =
  match (map_lookup klass_name map) with
    | Some(method_map) -> map_lookup_list func_name method_map
    | _ -> []

(* Recursively build the ancestor map for each class
 * Builds a map such that for each class name a list of ancestors is returned. The first
 * element is the 0th ancestor, the class itself. The last ancestor is Object -- in between
 * the ancestors go from child to parent
 *)
let build_ancestor_map parent_map =
  let rec ancestor_builder klass map =
    if StringMap.mem klass map then map
    else
      let parent = StringMap.find klass parent_map in
      let map = ancestor_builder parent map in
      let ancestors = StringMap.find parent map in
      StringMap.add klass (klass::ancestors) map in
  let folder klass _ map = ancestor_builder klass map in
  let map = StringMap.add "Object" ["Object"] StringMap.empty in
  StringMap.fold folder parent_map map

(* Given a klass and its list of ancestors, build a distance map to those
 * ancestors -- i.e. the number of hops back up the tree one must take to
 * get to any of those ancestors (assumes they are given in order)
 *)
let build_distance klass ancestors =
  let map_builder (map, i) item = (StringMap.add item i map, i+1) in
  fst (List.fold_left map_builder (StringMap.empty, 0) ancestors)

(* Given an ancestor map (see build_ancestor_map), build a distance map
 * out of the keys and values
 *)
let build_distance_map ancestor_map = StringMap.mapi build_distance ancestor_map

(* Given a distance map and two classes, return the distance between the two classes.
 * If one is a proper subtype of the other then either a Some(n) where n is non-zero
 * is returned; if they are the same then Some(0) is returned. If incomparable, then
 * None is returnd.
 *
 * Note that a non-negative is returned if klass1 derives transitively from klass2
 * and a non-positive is returned if klass2 derives transitively from klass1
 *)
let get_distance distance_map klass1 klass2 =
  (* We lt these pop exceptions because that means bad programming on the compiler
   * writers part, not on the GAMMA programmer's part (when klass1, klass2 aren't found)
   *)
  let klass1_map = StringMap.find klass1 distance_map in
  let klass2_map = StringMap.find klass2 distance_map in
  match map_lookup klass2 klass1_map, map_lookup klass1 klass2_map with
    | None, None -> None
    | None, Some(n) -> Some(-n)
    | res, _ ->  res

(* Given a distance map, determine if one type is a subtype of the other. Note that
 * this holds when the distance is zero and so when the two types are the same.
 *)
let is_subtype distance_map subtype supertype =
  match get_distance distance_map subtype supertype with
    | Some(n) when n >= 0 -> true
    | _ -> false

(* Given a distance map, determine if one type is a strict subtype of the other. Note
 * that this means tha the two types are not the same.
 *)
let is_proper_subtype distance_map subtype supertype =
  match get_distance distance_map subtype supertype with
    | Some(n) when n > 0 -> true
    | _ -> false

(* Return whether a formals list is compatible with an actuals list
 * This includes checking that the number of arguments are the same, too.
 *)
let compatible_arguments distance_map actuals formals =
  let compatible formal actual = is_subtype distance_map actual formal in
  try List.for_all2 compatible formals actuals with
    | Invalid_argument(_) -> false

(* A predicate for whether a func_def is compatible with an actuals list *)
let is_compatible distance_map actuals func_def =
  compatible_arguments distance_map actuals (List.map fst func_def.formals)

(* Given a list of compatible methods, return Some(func_def) that is the `best' match
 * lexicographically given the distance map and the actuals
 *)
(*
let best_match distance_map actuals func_defs =
  let funcs_formals = List.map (function func_def -> (func_def, func_def.formals)) func_defs in
  let update_formals = function
    | (func, []) -> raise(Failure("Compatible methods don't have enough parameters -- Compiler error!"))
    | (func, _::formals) -> (func, formals) in

  let rec find_best_match remaining_actuals = function
    | [] -> None
    | [func] -> Some(func)
    | funcs -> match remaining_actuals with
      | [] -> raise(Failure("Compiler error -- lexicographic selection not unique, must have duplicate method signatures."))
      | actual::rest ->
        let actual_map = StringMap.find actual distance_map in
        let distance (_, formal::_) = StringMap.find formal actual_map in
        let min_dist = min (List.map distance funcs) in
        let funcs = List.filter (function func -> (distance func) == min_dist) funcs in
        find_best_match rest (List.map update_formals funcs) in

  find_best_match actuals func_formals
*)

