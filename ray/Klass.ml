open Ast
open Util
open StringModules

(** Approximates a class *)

(** A full class record table as a type *)
type class_data = {
  (** A map that goes from class names to to class definition records *)
  classes : class_def lookup_map;

  (**
      A map that goes from class names to parents --
      everything should have a parent except Object
    *)
  parents : string lookup_map;

  (** A map that goes from class names to a list of children *)
  children : (string list) lookup_map;

  (**
      A table [map -> map -> value]; primary key is the class name;
      secondary key is variable name; result is a pair: the section
      variable was declared in and its type
    *)
  variables : (class_section * string) lookup_table;

  (**
      A table [map -> map -> value]; primary key is the class name;
      secondary key is the method name; result is a list of functions
      that have the given name in the given class
    *)
  methods : (func_def list) lookup_table;

  (**
      A table [map -> map -> value]; primary key is the class name;
      secondary key is the refinement `host.refinement'; the result is
      a list of functions that have the given name in the given class
    *)
  refines : (func_def list) lookup_table;

  (** A map from class names to main functions *)
  mains : func_def lookup_map;

  (**
      A map from class names to the list of ancestors -- the list runs
      from the given class back to Object in the expected fashion
    *)
  ancestors : (string list) lookup_map;

  (**
      A table [map -> map -> value]; primary key is subtype; secondary
      key is the ancestor; result is the positive integer distance that
      represents how many hops along the class tree are between the two
      classes. Distance from a type to itself is 0.
    *)
  distance : int lookup_table;
}

(** Construct an empty class_data object *)
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

(**
    From a class get the parent
    @param aklass is a class_def to get the parent of
    @return The name of the parent object
  *)
let klass_to_parent aklass = match aklass with
  | { parent = None; _ } -> "Object"
  | { parent = Some(aklass); _ } -> aklass

(**
    Utility function -- place variables in left, methods (including init) in right
    @param mem A member_def value (VarMem, MethodMem, InitMem)
    @return Places the values held by VarMem in Left, values held by MethodMem or InitMem in Right
  *)
let member_split mem = match mem with
  | VarMem(v) -> Left(v)
  | MethodMem(m) -> Right(m)
  | InitMem(i) -> Right(i)


(**
    Stringify a section to be printed
    @param section A class_section value (Privates, Protects, Publics, Refines, or Mains)
    @return The stringification of the section for printing
  *)
let section_string section = match section with
  | Privates -> "private"
  | Protects -> "protected"
  | Publics -> "public"
  | Refines -> "refinement"
  | Mains -> "main"

(**
    Return the variables of the class
    @param aklass The class to explore
    @return A list of ordered pairs representing different sections,
    the first item of each pair is the type of the section, the second
    is a map of the variables. Note that this only returns pairs for
    Publics, Protects, and Privates as the others cannot have variables
  *)
let klass_to_variables aklass =
  let vars members = fst (either_split (List.map member_split members)) in
  let s = aklass.sections in
  [(Publics, vars s.publics); (Protects, vars s.protects); (Privates, vars s.privates)]

(**
    Return the methods of the class
    @param aklass The class to explore
    @return A list of ordered pairs representing different sections,
    the first item of each pair is the type of the section, the second
    is a map of the methods. Note that this only returns the methods
    in Publics, Protects, or Privates as the other sections don't have
    `normal' methods in them
  *)
let klass_to_methods aklass =
  let funcs members = snd (either_split (List.map member_split members)) in
  let s = aklass.sections in
  [(Publics, funcs s.publics); (Protects, funcs s.protects); (Privates, funcs s.privates)]

(**
    Get anything that is invocable, not just instance methods
    @param aklass The class to explore
    @return The combined list of refinements, mains, and methods
  *)
let klass_to_functions aklass =
  let s = aklass.sections in
  (Refines, s.refines) :: (Mains, s.mains) :: klass_to_methods aklass

(**
    Add the children map
    ( parent name (string) -> children names (string list) )
    to a class_data record
    @param data A class_data record
    @param klasses A list of parsed classes
    @return data but with the children map updated given klasses.
  *)
let build_children_map data klasses =
  let map_builder map aklass = add_map_list (klass_to_parent aklass) (aklass.klass) map in
  let children_map = List.fold_left map_builder StringMap.empty klasses in
  { data with children = children_map }

(**
    Add the parent map
    ( child name (string) -> parent name (string) )
    to a class_data record
    @param data A class_data record
    @param klasses A list of parsed classes
    @return data but with the children map added in given klasses
  *)
let build_parent_map data klasses =
  let map_builder map aklass =
    let parent = klass_to_parent aklass in
    let child  = aklass.klass in
    StringMap.add child parent map in
  let parent_map = List.fold_left map_builder StringMap.empty klasses in
  { data with parents = parent_map }

(**
    Validate that the parent map in a class_data record represents a tree rooted at object.
    @param data a class_data record
    @return An optional string (Some(string)) when there is an issue.
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

(**
    Add the class map
    ( class name (string) -> class (class_def) )
    to a class_data record
    @param data A class_data record to update
    @klasses A list of parsed classes
    @return data but with the class map added given klasses
  *)
let build_class_map data klasses =
  let map_builder map aklass = add_map_unique (aklass.klass) aklass map in
  match build_map_track_errors map_builder klasses with
    | Left(class_map) -> Left({ data with classes = class_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(**
    For a given class, build a map of variable names to variable information.
    If all instance variables are uniquely named, returns Left (map) where map
    is  var name -> (class_section, type)  otherwise returns Right (collisions)
    where collisions are the names of variables that are multiply declared.
    @param aklass A parsed class
    @return a map of instance variables in the class
  *)
let build_var_map aklass =
  let add_var section map (typeId, varId) = add_map_unique varId (section, typeId) map in
  let map_builder map (section, members) = List.fold_left (add_var section) map members in
  build_map_track_errors map_builder (klass_to_variables aklass)

(**
    Add the variable map
    ( class name (string) -> variable name (string) -> variable info (class_section, type) )
    to a class_data record
    @param data A class_data record
    @param klasses A list of parsed classes
    @return Either a list of collisions (in Right) or the updated record (in Left).
    Collisions are pairs (class name, collisions (var names) for that class)
  *)
let build_class_var_map data klasses =
  let map_builder (klass_map, collision_list) aklass =
    match build_var_map aklass with
      | Left(var_map) -> (StringMap.add (aklass.klass) var_map klass_map, collision_list)
      | Right(collisions) -> (klass_map, (aklass.klass, collisions)::collision_list) in
  match build_map_track_errors map_builder klasses with
    | Left(variable_map) -> Left({ data with variables = variable_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(**
    Return whether two function definitions have conflicting signatures
    @param func1 A func_def
    @param func2 A func_def
    @return Whether the functions have the same name and the same parameter type sequence
  *)
let conflicting_signatures func1 func2 =
  let same_type (t1, _) (t2, _) = (t1 = t2) in
  let same_name = (func1.name = func2.name) in
  let same_params = try List.for_all2 same_type func1.formals func2.formals with
    | Invalid_argument(_) -> false in
  same_name && same_params

(**
    Return a string that describes a function
    @param func A func_def
    @return A string showing the simple signature ([host.]name and arg types)
  *)
let signature_string func =
  let name = match func.host with
    | None -> func.name
    | Some(h) -> Format.sprintf "%s.%s" h func.name in
  Format.sprintf "%s(%s)" name (String.concat ", " (List.map fst func.formals))

(**
    Return a string representing the full signature of the function
    @param func A func_def
    @return A string showing the signature (section, [host.]name, arg types)
  *)
let full_signature_string func =
  let ret = match func.returns with
    | None -> "Void"
    | Some(t) -> t in
  Format.sprintf "%s %s %s" (section_string func.section) ret (signature_string func)

(**
    Build a map of all the methods within a class, returing either a list of collisions
    (in Right) when there are conflicting signatures or the map (in Left) when there
    are not. Keys to the map are function names and the values are lists of func_def's.
    @param aklass A klass to build a method map for
    @return Either a list of collisions or a map of function names to func_def's.
  *)
let build_method_map aklass =
  let add_method (map, collisions) fdef =
    if List.exists (conflicting_signatures fdef) (map_lookup_list fdef.name map)
      then (map, fdef::collisions)
      else (add_map_list fdef.name fdef map, collisions) in
  let map_builder map funcs = List.fold_left add_method map funcs in
  build_map_track_errors map_builder (List.map snd (klass_to_methods aklass))

(**
    Add the class name (string) -> method name (string) -> methods  (func_def list)
    methods table to a class_data record, given a list of classes. If there are no
    collisions, the updated record is returned (in Left), otherwise the collision
    list is returned (in Right).
    @param data A class data record
    @param klasses A list of parsed classes
    @return Either a list of collisions (in Right) or the updated record (in Left).
    Collisions are pairs (class name, colliding methods for that class). Methods collide
    if they have conflicting signatures (ignoring return type).
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

(**
    Build the map of refinements for a given class. Keys to the map are `host.name'
    @param aklass aklass A class to build a refinement map out of
    @return Either a list of collisions (in Right) or the map (in left). Refinements
    conflict when they have the same name (`host.name' in this case) and have the same
    argument type sequence.
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

(**
    Add the class name (string) -> refinement (`host.name' - string) -> func list
    map to a class_data record. If there are no collisions (conflicting signatures
    given the same host), then the updated record is returned (in Left) otherwise
    a list of collisions is returned (in Right).
    @param data A class_data record
    @param klasses A list of parsed classes
    @return either a list of collisions (in Right) or the updated record (in Left).
    Collisions are (class, (host, method, formals) list)
  *)
let build_class_refinement_map data klasses =
  let to_collision func = match func.host with
    | None -> raise(Invalid_argument("Cannot build refinement collisions -- refinement without host [compiler error]."))
    | Some(host) -> (host, func.name, List.map fst func.formals) in
  let to_collisions funcs = List.map to_collision funcs in
  let map_builder (klass_map, collision_list) aklass =
    match build_refinement_map aklass with
      | Left(refinement_map) -> (StringMap.add aklass.klass refinement_map klass_map, collision_list)
      | Right(collisions) -> (klass_map, (aklass.klass, to_collisions collisions)::collision_list) in
  match build_map_track_errors map_builder klasses with
    | Left(refinement_map) -> Left({ data with refines = refinement_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(**
    Add a map of main functions, from class name (string) to main (func_def) to the
    class_data record passed in. Returns a list of collisions if any class has more
    than one main (in Right) or the updated record (in Left)
    @param data A class_data record
    @param klasses A list of parsed classes
    @return Either the collisions (Right) or the updated record (Left)
  *)
let build_main_map data klasses =
  let add_klass (map, collisions) aklass = match aklass.sections.mains with
    | [] -> (map, collisions)
    | [main] -> (StringMap.add aklass.klass main map, collisions)
    | _ -> (map, aklass.klass :: collisions) in
  match build_map_track_errors add_klass klasses with
    | Left(main_map) -> Left({ data with mains = main_map })
    | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(**
    Look up an instance variable given a class_data record, a class name, and a variable name
    @return The information for that instance variable in that class if it exists.
*)
let class_var_lookup data klass_name var_name =
  match map_lookup klass_name data.variables with
    | Some(var_map) -> map_lookup var_name var_map
    | _ -> None

(**
    Look up the methods of a class given a class_data record, a class name, and a method name,
    @return A list of methods in that class with that name or the empty list if no such method exists.
*)
let class_method_lookup data klass_name func_name =
  match map_lookup klass_name data.methods with
    | Some(method_map) -> map_lookup_list func_name method_map
    | _ -> []

(**
    Given a class_data record, add the ancestor map ( class name (string) -> ancestors (string list) )
    to the record; note that the ancestors start with the given class and end with Object (or should).
    the ancestors should be given in order.
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

(**
    Given a klass and its list of ancestors, build a distance map to those
    ancestors -- i.e. the number of hops back up the tree one must take to
    get to any of those ancestors (assumes they are given in order)
*)
let build_distance klass ancestors =
  let map_builder (map, i) item = (StringMap.add item i map, i+1) in
  fst (List.fold_left map_builder (StringMap.empty, 0) ancestors)

(**
    Given a class_data record, add the distance map for the record. The distance map takes a sub class
    and then an ancestor class and tells you the distance between the two. (requires ancestor map built)
*)
let build_distance_map data =
  let distance_map = StringMap.mapi build_distance data.ancestors in
  { data with distance = distance_map }

(**
    Given a class_data record and two classes, returns the distance between them. If one is a proper
    subtype of the other then Some(n) is returned where n is non-zero -- zero is returned when the
    two are the same class. If incomparable then None is returned

    A non-negative value is returned if the first class is a subtype of the second; otherwise a
    non-positive value is returned if the second is a subtype of the first.
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

(** Check if a type exists in the class data -- convenience function *)
let is_type data atype = match map_lookup atype data.classes with
  | None -> false
  | _ -> true

(**
    Check if a class is a subclass of another given a class_data record
    @return Whether the first type is a subtype of the second
    Note that this holds when the two types are identical
*)
let is_subtype data subtype supertype =
  match get_distance data subtype supertype with
    | Some(n) when n >= 0 -> true
    | _ -> false

(**
    Check if a type is proper subtype of a second given a class_data record
    @return Whether the first type is a proper subtype of the second.
 *)
let is_proper_subtype data subtype supertype =
  match get_distance data subtype supertype with
    | Some(n) when n > 0 -> true
    | _ -> false

(**
    Return whether a formals list is compatible with an actuals list
    This includes checking that the number of arguments are the same, too.
*)
let compatible_formals data actuals formals =
  let compatible formal actual = is_subtype data actual formal in
  try List.for_all2 compatible formals actuals with
    | Invalid_argument(_) -> false

(** A predicate for whether a func_def is compatible with an actuals list *)
let compatible_function data actuals func =
  compatible_formals data actuals (List.map fst func.formals)

(**
    Filter a list of functions based on their section.
    @param funcs a list of functions
    @param sects a list of class_section values
    @return a list of functions in the given sections
  *)
let in_section sects funcs =
  List.filter (fun f -> List.mem f.section sects) funcs

(**
    Given a class_data record, a list of actual arguments, and a list of methods,
    find the best matches for the actuals. Note that if there are multiple best
    matches (i.e. ties) then a non-empty non-singleton list is returned.

    Raises an error if somehow our list of compatible methods becomes incompatible
    [i.e. there is a logic error in the compiler].
*)
let best_matching_signature data actuals funcs sections =
  let funcs = List.filter (compatible_function data actuals) funcs in
  let distance_of actual formal = match get_distance data actual formal with
    | Some(n) when n >= 0 -> n
    | _ -> raise(Invalid_argument("Compatible methods somehow incompatible: " ^ actual ^ " vs. " ^ formal ^ ". Compiler error.")) in
  let to_distance func = List.map2 distance_of actuals (List.map fst func.formals) in
  let with_distances = List.map (fun func -> (func, to_distance func)) funcs in
  let lex_compare (_, lex1) (_, lex2) = lexical_compare lex1 lex2 in
  List.map fst (find_all_min lex_compare with_distances)

(**
    Given a class_data record, method name, and list of actuals, get the best matching
    method. Note that if there is more than one then an exception is raised as this should
    have been reported during collision detection [compiler error].
*)
let best_method data klass_name method_name actuals sections =
  let methods = class_method_lookup data klass_name method_name in
  let methods = in_section sections methods in
  match best_matching_signature data actuals methods sections with
    | [] -> None
    | [func] -> Some(func)
    | _ -> raise(Invalid_argument("Multiple methods of the same signature in " ^ klass_name ^ "; Compiler error."))

type class_data_error
  = HierarchyIssue of string
  | DuplicateClasses of string list
  | DuplicateVariables of (string * string list) list
  | ConflictingMethods of (string * (string * string list) list) list
  | ConflictingRefinements of (string * (string * string * string list) list) list
  | MultipleMains of string list

(**
    Append functions build the class_data record (Left) if successful,
    the error stack if unsuccessful (Right)
*)


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

(**
    Build it up
*)
let build_class_data klasses
  =  Left(empty_data)
  |-> append_children klasses
  |-> append_parent klasses
  |-> test_tree
  |-> append_classes klasses
  |-> append_variables klasses
  |-> append_methods klasses
  |-> append_refines klasses
  |-> append_mains klasses
  |-> append_ancestor
  |-> append_distance

let print_class_data data =
  let id x = x in
  let from_list lst = Format.sprintf "[%s]" (String.concat ", " lst) in
  let table_printer tbl name stringer =
    let printer p s i = Format.sprintf "\t%s : %s => %s\n" p s (stringer i) in
    print_string (name ^ ":\n");
    print_lookup_table tbl printer in
  let map_printer map name stringer =
    let printer k i = Format.sprintf "\t%s => %s\n" k (stringer i) in
    print_string (name ^ ":\n");
    print_lookup_map map printer in

  let func_list funcs =
    let sigs = List.map (fun f -> "\n\t\t" ^ (full_signature_string f)) funcs in
    String.concat "" sigs in

  let class_printer cdef =
    let rec count sect = function
      | (where, members)::_ when where = sect -> List.length members
      | _::rest -> count sect rest
      | [] -> raise(Failure("The impossible happened -- searching for a section that should exist doesn't exist.")) in
    let vars = klass_to_variables cdef in
    let funcs = klass_to_functions cdef in
    let format = "class %s extends %s and has\n" ^^
                 "\t\t(%d/%d/%d) methods -- private, protected, public\n" ^^
                 "\t\t(%d/%d/%d) fields -- private, protected, public\n" ^^
                 "\t\t%d refinements, %d mains" in
    Format.sprintf format cdef.klass (klass_to_parent cdef)
                   (count Privates funcs) (count Protects funcs) (count Publics funcs)
                   (count Refines funcs) (count Mains funcs)
                   (count Privates vars) (count Protects vars) (count Publics vars) in

  map_printer data.classes "Classes" class_printer;
  print_newline ();
  map_printer data.parents "Parents" id;
  print_newline ();
  map_printer data.children "Children" from_list;
  print_newline ();
  table_printer data.variables "Fields" (fun (sect, t) -> Format.sprintf "%s %s" (section_string sect) t);
  print_newline ();
  table_printer data.methods "Methods" func_list;
  print_newline ();
  table_printer data.refines "Refines" func_list;
  print_newline ();
  map_printer data.mains "Mains" full_signature_string;
  print_newline ();
  map_printer data.ancestors "Ancestors" from_list;
  print_newline ();
  table_printer data.distance "Distance" string_of_int
