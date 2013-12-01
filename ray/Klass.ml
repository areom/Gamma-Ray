open Ast
open Util
module StringMap = Map.Make (String)

(* Just for convenient reading *)
type 'a lookup_table = 'a StringMap.t StringMap.t
type 'a lookup_map = 'a StringMap.t

type class_data = {
  classes : class_def lookup_map;
  parents : string lookup_map;
  children : (string list) lookup_map;
  variables : (class_section * string) lookup_table;
  methods : (func_def list) lookup_table;
  refines : (func_def list) lookup_table;
  mains : func_def lookup_map;
  ancestors : (string list) lookup_map;
  distance : int lookup_table;
}

let empty_data : class_data = {
  classes = StringMap.empty;
  parents = StringMap.empty;
  children = StringMap.empty;
  variables = StringMap.empty;
  methods = StringMap.empty;
  refines = StringMap.empty;
  mains = StringMap.empty;
  ancestors = StringMap.empty;
  distance = StringMap.empty;
}

(* Builder should accept a StringMap, errors list pair, a new item, and either return an
 * updated map or an updated error list in the new pair (but hopefully not both). alist
 * is the list of data to build the map out of. So, to put it into symbols, we have
 *   builder : (StringMap, errorList) -> item -> (StringMap', errorList')
 *)
let build_map_track_errors builder alist =
  match List.fold_left builder (StringMap.empty, []) alist with
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

(* Go from a class to a list of instance variables in that class, separated by section *)
let klass_to_variables aklass =
  let to_variable = function
    | VarMem(v) -> Some(v)
    | _ -> None in
  let vars members = filter_option (List.map to_variable members) in
  let s = aklass.sections in
  [(Publics, vars s.publics); (Protects, vars s.protects); (Privates, vars s.privates)]

(* Go from a class to a list of instance methods in that class, separated by section *)
let klass_to_methods aklass =
  let to_function = function
    | MethodMem(m) -> Some(m)
    | InitMem(i) -> Some(i)
    | _ -> None in
  let funcs members = filter_option (List.map to_function members) in
  let s = aklass.sections in
  [(Publics, funcs s.publics); (Protects, funcs s.protects); (Privates, funcs s.privates)]

(* Get anything that is invocable, not just instance methods *)
let klass_to_functions aklass =
  let s = aklass.sections in
  (Refines, s.refines) :: (Mains, s.mains) :: klass_to_methods aklass

(* Add the children map
 *   ( parent name (string) -> children names (string list) )
 * to a class_data record
 *)
let build_children_map data klasses =
  let map_builder map aklass = add_map_list (klass_to_parent aklass) (aklass.klass) map in
  let children_map = List.fold_left map_builder StringMap.empty klasses in
  { data with children = children_map }

(* Add the parent map
 *   ( child name (string) -> parent name (string) )
 * to a class_data record
 *)
let build_parent_map data klasses =
  let map_builder map aklass =
    let parent = klass_to_parent aklass in
    let child  = aklass.klass in
    StringMap.add parent child map in
  let parent_map = List.fold_left map_builder StringMap.empty klasses in
  { data with parents = parent_map }

(* Validate that the parent map in a class_data record represents a tree rooted at object. *
 * Returns an optional string (Some(string)) when there is an issue.
 *)
let is_tree_hierarchy data =
  let rec from_object klass checked =
    match map_lookup klass checked with
      | Some(true) -> Left(checked)
      | Some(false) -> Right("Cycle detected.")
      | _ -> match map_lookup klass data.parents with
        | None -> Right("Cannot find parent after building parent map: " ^ klass)
        | Some(parent) -> match from_object parent (StringMap.add klass false checked) with
          | Left(updated) -> Left(StringMap.add klass true updated)
          | issue -> issue in
  let folder klass _ = function
    | Left(checked) -> from_object klass checked
    | issue -> issue in
  let checked = StringMap.add "Object" true StringMap.empty in
  match StringMap.fold folder data.parents (Left(checked)) with
    | Right(issue) -> Some(issue)
    | _ -> None

(* Add the class map
 *   ( class name (string) -> class (class_def) )
 * to a class_data record
 *)
let build_class_map data klasses =
  let map_builder map aklass = add_map_unique (aklass.klass) aklass map in
  match build_map_track_errors map_builder klasses with
    | Left(class_map) -> Left({ data with classes = class_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(* For a given class, build a map of variable names to variable information.
 * If all instance variables are uniquely named, returns Left (map) where map
 * is  var name -> (class_section, type)  otherwise returns Right (collisions)
 * where collisions are the names of variables that are multiply declared.
 *)
let build_var_map aklass =
  let add_var section map (typeId, varId) = add_map_unique varId (section, typeId) map in
  let map_builder map (section, members) = List.fold_left (add_var section) map members in
  build_map_track_errors map_builder (klass_to_variables aklass)

(* Add the variable map
 *   ( class name (string) -> variable name (string) -> variable info (class_section, type) )
 * to a class_data record
 * Returns either a list of collisions (in Right) or the updated record (in Left).
 * Collisions are pairs (class name, collisions (var names) for that class)
 *)
let build_class_var_map data klasses =
  let map_builder (klass_map, collision_list) aklass =
    match build_var_map aklass with
      | Left(var_map) -> (StringMap.add (aklass.klass) var_map klass_map, collision_list)
      | Right(collisions) -> (klass_map, (aklass.klass, collisions)::collision_list) in
  match build_map_track_errors map_builder klasses with
    | Left(variable_map) -> Left({ data with variables = variable_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(* Return whether two function definitions have conflicting signatures *)
let conflicting_signatures func1 func2 =
  let same_type (t1, _) (t2, _) = (t1 = t2) in
  let same_name = (func1.name = func2.name) in
  let same_params = try List.for_all2 same_type func1.formals func2.formals with
    | Invalid_argument(_) -> false in
  same_name && same_params

let signature_string func_def =
  Format.sprintf "%s(%s)" func_def.name (String.concat ", " (List.map fst func_def.formals))

(* Builds a map of all the methods within a class, returning a list of collisions
 * when there are conflicting signatures. Keys to the map are function names and
 * the values are list of func_def pairs.
 *)
let build_method_map aklass =
  let add_method (map, collisions) fdef =
    if List.exists (conflicting_signatures fdef) (map_lookup_list fdef.name map)
      then (map, fdef::collisions)
      else (add_map_list fdef.name fdef map, collisions) in
  let map_builder map funcs = List.fold_left add_method map funcs in
  build_map_track_errors map_builder (List.map snd (klass_to_methods aklass))

(* Add the method map
 *   ( class name (string) -> method name (string) -> methods (func_def list) )
 * to the class_data record.
 * Returns either a list of collisions (in Right) or the updated record (in Left).
 * Collisions are pairs (class name, colliding methods for that class). Methods collide
 * if they have conflicting signatures (ignoring return type).
 *)
let build_class_method_map data klasses =
  let to_collision func = (func.name, List.map fst func.formals) in
  let to_collisions funcs = List.map to_collision funcs in
  let map_builder (klass_map, collision_list) aklass =
    match build_method_map aklass with
      | Left(method_map) -> (StringMap.add (aklass.klass) method_map klass_map, collision_list)
      | Right(collisions) -> (klass_map, (aklass.klass, to_collisions collisions)::collision_list) in
  match build_map_track_errors map_builder klasses with
    | Left(method_map) -> Left({ data with methods = method_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(* Build the map of refinements for a given class, returning a list of collisions
 * when there are conflicting signatures. Keys to the map are "host.func"
 *)
let build_refinement_map aklass =
  let add_refinement (map, collisions) func = match func.host with
    | Some(host) ->
      let key = func.name ^ "." ^ host in
      if List.exists (conflicting_signatures func) (map_lookup_list key map)
        then (map, func::collisions)
        else (add_map_list key func map, collisions)
    | None -> raise(Failure("Compilation error -- non-refinement found in searching for refinements.")) in
  build_map_track_errors add_refinement aklass.sections.refines

(* Build the map of classes to refinements -- class names are the first key, host.name is the second,
 * results are lists of methods.
 *)
let build_class_refinement_map data klasses =
  let to_collision func = (func.host, func.name, List.map fst func.formals) in
  let to_collisions funcs = List.map to_collision funcs in
  let map_builder (klass_map, collision_list) aklass =
    match build_refinement_map aklass with
      | Left(refinement_map) -> (StringMap.add aklass.klass refinement_map klass_map, collision_list)
      | Right(collisions) -> (klass_map, (aklass.klass, to_collisions collisions)::collision_list) in
  match build_map_track_errors map_builder klasses with
    | Left(refinement_map) -> Left({ data with refines = refinement_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(* Build the map of mains -- class name to main *)
let build_main_map data klasses =
  let add_klass (map, collisions) aklass =  match aklass.sections.mains with
    | [] -> (map, collisions)
    | [main] -> (StringMap.add aklass.klass main map, collisions)
    | _ -> (map, aklass.klass :: collisions) in
  match build_map_track_errors add_klass klasses with
    | Left(main_map) -> Left({ data with mains = main_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(* Given a class_data record, a class name, and a variable name, return the information for
 * that instance variable in that class if it exists.
 *)
let class_var_lookup data klass_name var_name =
  match map_lookup klass_name data.variables with
    | Some(var_map) -> map_lookup var_name var_map
    | _ -> None

(* Given a class_data record, a class name, and a method name, return a list of methods
 * in that class with that name. Returns the empty list if no such method exists.
 *)
let class_method_lookup data klass_name func_name =
  match map_lookup klass_name data.methods with
    | Some(method_map) -> map_lookup_list func_name method_map
    | _ -> []

(* Given a class_data record, add the ancestor map ( class name (string) -> ancestors (string list) )
 * to the record; note that the ancestors start with the given class and end with Object (or should).
 * The ancestors should be given in order.
 *)
let build_ancestor_map data =
  let rec ancestor_builder klass map =
    if StringMap.mem klass map then map
    else
      let parent = StringMap.find klass data.parents in
      let map = ancestor_builder parent map in
      let ancestors = StringMap.find parent map in
      StringMap.add klass (klass::ancestors) map in
  let folder klass _ map = ancestor_builder klass map in
  let map = StringMap.add "Object" ["Object"] StringMap.empty in
  let ancestor_map = StringMap.fold folder data.parents map in
  { data with ancestors = ancestor_map }

(* Given a klass and its list of ancestors, build a distance map to those
 * ancestors -- i.e. the number of hops back up the tree one must take to
 * get to any of those ancestors (assumes they are given in order)
 *)
let build_distance klass ancestors =
  let map_builder (map, i) item = (StringMap.add item i map, i+1) in
  fst (List.fold_left map_builder (StringMap.empty, 0) ancestors)

(* Given a class_data record, add the distance map for the record. The distance map takes a sub class
 * and then an ancestor class and tells you the distance between the two. (requires ancestor map built)
 *)
let build_distance_map data = 
  let distance_map = StringMap.mapi build_distance data.ancestors in
  { data with distance = distance_map }

(* Given a class_data record and two classes, returns the distance between them. If one is a proper
 * subtype of the other then Some(n) is returned where n is non-zero -- zero is returned when the
 * two are the same class. If incomparable then None is returned.
 *
 * A non-negative value is returned if the first class is a subtype of the second; otherwise a
 * non-positive value is returned if the second is a subtype of the first.
 *)
let get_distance data klass1 klass2 =
  (* We lt these pop exceptions because that means bad programming on the compiler
   * writers part, not on the GAMMA programmer's part (when klass1, klass2 aren't found)
   *)
  let klass1_map = StringMap.find klass1 data.distance in
  let klass2_map = StringMap.find klass2 data.distance in
  match map_lookup klass2 klass1_map, map_lookup klass1 klass2_map with
    | None, None -> None
    | None, Some(n) -> Some(-n)
    | res, _ ->  res

(* Given a class_data record return whether the first type is a subtype of the second
 * Note that this holds when the two types are identical
 *)
let is_subtype data subtype supertype =
  match get_distance data subtype supertype with
    | Some(n) when n >= 0 -> true
    | _ -> false

(* Given a class_data record return whether the first type is a subtype of the second
 * and they are not the same type.
 *)
let is_proper_subtype data subtype supertype =
  match get_distance data subtype supertype with
    | Some(n) when n > 0 -> true
    | _ -> false

(* Return whether a formals list is compatible with an actuals list
 * This includes checking that the number of arguments are the same, too.
 *)
let compatible_formals data actuals formals =
  let compatible formal actual = is_subtype data actual formal in
  try List.for_all2 compatible formals actuals with
    | Invalid_argument(_) -> false

(* A predicate for whether a func_def is compatible with an actuals list *)
let compatible_function data actuals func_def =
  compatible_formals data actuals (List.map fst func_def.formals)

(* Given a class_data record, a list of actual arguments, and a list of methods,
 * find the best matches for the actuals. Note that if there are multiple best
 * matches (i.e. ties) then a non-empty non-singleton list is returned.
 *
 * Raises an error if somehow our list of compatible methods becomes incompatible
 * [i.e. there is a logic error in the compiler].
 *)
let best_matching_signature data actuals funcs =
  let funcs = List.filter (compatible_function data actuals) funcs in
  let distance_of actual formal = match get_distance data actual formal with
    | Some(n) when n >= 0 -> n
    | _ -> raise(Invalid_argument("Compatible methods somehow incompatible: " ^ actual ^ " vs. " ^ formal ^ ". Compiler error.")) in
  let to_distance func = List.map2 distance_of actuals (List.map fst func.formals) in
  let with_distances = List.map (function func -> (func, to_distance func)) funcs in
  let lex_compare (_, lex1) (_, lex2) = lexical_compare lex1 lex2 in
  List.map fst (find_all_min lex_compare with_distances)

(* Given a class_data record, method name, and list of actuals, get the best matching
 * method. Note that if there is more than one then an exception is raised as this should
 * have been reported during collision detection [compiler error].
 *)
let best_method data klass_name method_name actuals =
  let methods = class_method_lookup data klass_name method_name in
  match best_matching_signature data actuals methods with
    | [] -> None
    | [func] -> Some(func)
    | _ -> raise(Invalid_argument("Multiple methods of the same signature in " ^ klass_name ^ "; Compiler error."))

type class_data_error
  = HierarchyIssue of string
  | DuplicateClasses of string list
  | DuplicateVariables of (string * string list) list
  | ConflictingMethods of (string * (string * string list) list) list
  | ConflictingRefinements of (string * (string option * string * string list) list) list
  | MultipleMains of string list

let append_children klasses data = Left(build_children_map data klasses)
let append_parent klasses data = Left(build_parent_map data klasses)
let test_tree data = match is_tree_hierarchy data with
  | None -> Left(data)
  | Some(problem) -> Right(HierarchyIssue(problem))
let append_classes klasses data = match build_class_map data klasses with
  | Left(data) -> Left(data)
  | Right(collisions) -> Right(DuplicateClasses(collisions))
let append_variables klasses data = match build_class_var_map data klasses with
  | Left(data) -> Left(data)
  | Right(collisions) -> Right(DuplicateVariables(collisions))
let append_methods klasses data = match build_class_method_map data klasses with
  | Left(data) -> Left(data)
  | Right(collisions) -> Right(ConflictingMethods(collisions))
let append_refines klasses data = match build_class_refinement_map data klasses with
  | Left(data) -> Left(data)
  | Right(collisions) -> Right(ConflictingRefinements(collisions))
let append_mains klasses data = match build_main_map data klasses with
  | Left(data) -> Left(data)
  | Right(collisions) -> Right(MultipleMains(collisions))
let append_ancestor data = Left(build_ancestor_map data)
let append_distance data = Left(build_distance_map data)

let build_class_data klasses
  =  Left(empty_data)
  |> append_children klasses
  |> append_parent klasses
  |> test_tree
  |> append_classes klasses
  |> append_variables klasses
  |> append_methods klasses
  |> append_refines klasses
  |> append_mains klasses
  |> append_ancestor
  |> append_distance
