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
        Add the parent (class name - string) -> children (class name -> string list)
        map to a class_data record.
        @param data A class_data record
        @param klasses A list of parsed classes
        @return data but with the children map updated given klasses.
    *)
let build_children_map data klasses =
    let map_builder map aklass = add_map_list (klass_to_parent aklass) (aklass.klass) map in
    let children_map = List.fold_left map_builder StringMap.empty klasses in
    { data with children = children_map }

(**
        Add the child (class name - string) -> parent (class name - string) map to
        a class_data record.
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
        Add the class (class name - string) to definition (class - class_def)
        map to a class_data record.
        @param data A class_data record to update
        @param klasses A list of parsed classes
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
        Add the class (class name - string) -> variable (var name - string) -> info (section/type
        pair - class_section * string) table to a class_data record.
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
        Map function collisions to the type used for collection that information.
        This lets us have a `standard' form of method / refinement collisions and so
        we can easily build up a list of them.
        @param aklass the class we are currently examining
        @param funcs a list of funcs colliding in aklass
        @param reqhost are we requiring a host (compiler error if no host and true)
        @return a tuple representing the collisons - (class name, collision tuples)
          where collision tuples are ([host.]name, formals)
    *)
let build_collisions aklass funcs reqhost =
    let to_collision func =
        let name = match func.host, reqhost with
            | None, true -> raise(Invalid_argument("Cannot build refinement collisions -- refinement without host [compiler error]."))
            | None, _ -> func.name
            | Some(host), _ -> host ^ "." ^ func.name in
        (name, List.map fst func.formals) in
    (aklass.klass, List.map to_collision funcs)

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
    let map_builder (klass_map, collision_list) aklass =
        match build_method_map aklass with
            | Left(method_map) -> (StringMap.add (aklass.klass) method_map klass_map, collision_list)
            | Right(collisions) -> (klass_map, (build_collisions aklass collisions false)::collision_list) in
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
    let map_builder (klass_map, collision_list) aklass =
        match build_refinement_map aklass with
            | Left(refinement_map) -> (StringMap.add aklass.klass refinement_map klass_map, collision_list)
            | Right(collisions) -> (klass_map, (build_collisions aklass collisions true)::collision_list) in
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
        Given a class_data record, a class name, and a variable name, lookup the section and type
        info for that variable.
        @param data A class_data record
        @param klass_name The name of a class (string)
        @param var_name The name of a variable (string)
        @return Either None if the variable is not declared in the class or Some((section, type))
        where the variable is declared in section and has the given type.
    *)
let class_var_lookup data klass_name var_name =
    match map_lookup klass_name data.variables with
        | Some(var_map) -> map_lookup var_name var_map
        | _ -> None

(**
        Given a class_data record, a class name, and a method name, lookup all the methods in the
        given class with that name.
        @param data A class_data record
        @param klass_name The name of a class (string)
        @param func_name The name of a method (string)
        @return A list of methods in the class with that name or the empty list if no such method exists.
    *)
let class_method_lookup data klass_name func_name =
    match map_lookup klass_name data.methods with
        | Some(method_map) -> map_lookup_list func_name method_map
        | _ -> []

(**
        Add the class (class name - string) -> ancestors (list of ancestors - string list) map to a
        class_data record. Note that the ancestors go from `youngest' to `oldest' and so should start
        with the given class (hd) and end with Object (last item in the list).
        @param data The class_data record to update
        @return An updated class_data record with the ancestor map added.
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
        Given a class and a list of its ancestors, build a map detailing the distance
        between the class and any of its ancestors. The distance is the number of hops
        one must take to get from the given class to the ancestor. The distance between
        an Object and itself should be 0, and the largest distance should be to object.
        @param klass The class to build the table for
        @param ancestors The list of ancestors of the given class.
        @return A map from class names to integers
    *)
let build_distance klass ancestors =
    let map_builder (map, i) item = (StringMap.add item i map, i+1) in
    fst (List.fold_left map_builder (StringMap.empty, 0) ancestors)

(**
        Add a class (class name - string) -> class (class name - string) -> distance (int option)
        table a given class_data record. The distance is always a positive integer and so the
        first type must be either the same as the second or a subtype, else None is returned.
        Note that this requires that the ancestor map be built.
        @param data The class_data record to update.
        @return The class_data record with the distance map added.
    *)
let build_distance_map data =
    let distance_map = StringMap.mapi build_distance data.ancestors in
    { data with distance = distance_map }

(**
        Given a class_data record and two classes, returns the distance between them. If one is a proper
        subtype of the other then Some(n) is returned where n is non-zero when the two classes are different
        and comparable (one is a subtype of the other), zero when they are the same, and None when they are
        incomparable (one is not a subtype of the other)
        @param data A class_data record
        @param klass1 A class to check the relation of to klass2
        @param klass2 A class to check the relation of to klass1
        @return An int option, None when the two classes are incomparable, Some(positive) when klass2 is an
        ancestor of klass1, Some(negative) when klass1 is an ancestor of klass2.
    *)
let get_distance data klass1 klass2 =
    (* We let these pop exceptions because that means bad programming on the compiler
      * writers part, not on the GAMMA programmer's part (when klass1, klass2 aren't found)
      *)
    let klass1_map = StringMap.find klass1 data.distance in
    let klass2_map = StringMap.find klass2 data.distance in
    match map_lookup klass2 klass1_map, map_lookup klass1 klass2_map with
        | None, None -> None
        | None, Some(n) -> Some(-n)
        | res, _ ->  res

(**
        Check if a type exists in the class data -- convenience function
        @param data A class_data record
        @param atype The name of a class (string)
        @return True if the atype is a known type, false otherwise.
    *)
let is_type data atype = match map_lookup atype data.classes with
    | None -> false
    | _ -> true

(**
        Check if a class is a subclass of another given a class_data record
        @param data A class_data record
        @param subtype A class name (string)
        @param supertype A class name (string)
        @return Whether subtype has supertype as an ancestor given data.
        Note that this is true when the two are equal (trivial ancestor).
    *)
let is_subtype data subtype supertype =
    match get_distance data subtype supertype with
        | Some(n) when n >= 0 -> true
        | _ -> false

(**
        Check if a class is a proper subclass of another given a class_data record
        @param data A class_data record
        @param subtype A class name (string)
        @param supertype A class name (string)
        @return Whether subtype has supertype as an ancestor given data.
        Note that this IS NOT true when the two are equal (trivial ancestor).
    *)
let is_proper_subtype data subtype supertype =
    match get_distance data subtype supertype with
        | Some(n) when n > 0 -> true
        | _ -> false

(**
        Return whether a list of actuals and a list of formals are compatible.
        For this to be true, each actual must be a (not-necessarily-proper) subtype
        of the formal at the same position. This requires that both be the same
        in quantity, obviously.
        @param data A class_data record (has type information)
        @param actuals A list of the types (and just the types) of the actual arguments
        @param formals A list of the types (and just the types) of the formal arguments
        @return Whether the actual arguments are compatible with the formal arguments.
    *)
let compatible_formals data actuals formals =
    let compatible formal actual = is_subtype data actual formal in
    try List.for_all2 compatible formals actuals with
        | Invalid_argument(_) -> false

(**
        Return whether a given func_def is compatible with a list of actual arguments.
        This means making sure that it has the right number of formal arguments and that
        each actual agument is a subtype of the corresponding formal argument.
        @param data A class_data record (has type information)
        @param actuals A list of the types (and just the types) of the actual arguments
        @param func A func_def from which to get formals
        @return Whether the given func_def is compatible with the actual arguments.
    *)
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
        @param data A class_data record
        @param actuals The list of types (and only types) for the actual arguments
        @param funcs The list of candidate functions
        @return The list of all best matching functions (should be at most one, we hope).
    *)
let best_matching_signature data actuals funcs =
    let funcs = List.filter (compatible_function data actuals) funcs in
    let distance_of actual formal = match get_distance data actual formal with
        | Some(n) when n >= 0 -> n
        | _ -> raise(Invalid_argument("Compatible methods somehow incompatible: " ^ actual ^ " vs. " ^ formal ^ ". Compiler error.")) in
    let to_distance func = List.map2 distance_of actuals (List.map fst func.formals) in
    let with_distances = List.map (fun func -> (func, to_distance func)) funcs in
    let lex_compare (_, lex1) (_, lex2) = lexical_compare lex1 lex2 in
    List.map fst (find_all_min lex_compare with_distances)

(**
        Given a class_data record, method name, and list of actuals, and a list of sections to consider,
        get the best matching  method. Note that if there is more than one then an exception is raised
        as this should have been reported during collision detection [compiler error].
        @param data A class_data record
        @param method_name The name to lookup candidates for
        @param actuals The list of types (and only types) for the actual arguments
        @param sections The sections to filter on (only look in these sections)
        @return Either None if no function is found, Some(f) if one function is found, or an error is raised.
    *)
let best_method data klass_name method_name actuals sections =
    let methods = class_method_lookup data klass_name method_name in
    let methods = in_section sections methods in
    match best_matching_signature data actuals methods with
        | [] -> None
        | [func] -> Some(func)
        | _ -> raise(Invalid_argument("Multiple methods of the same signature in " ^ klass_name ^ "; Compiler error."))

(**
        All the different types of non-compiler errors that can occur (programmer errors)
    *)
type class_data_error
    = HierarchyIssue of string
    | DuplicateClasses of string list
    | DuplicateVariables of (string * string list) list
    | ConflictingMethods of (string * (string * string list) list) list
    | ConflictingRefinements of (string * (string * string list) list) list
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

let append_leaf_classes aklass data =
    let updated = StringMap.add aklass.klass aklass data.classes in
    if StringMap.mem aklass.klass data.classes
        then Right(DuplicateClasses([aklass.klass]))
        else Left({ data with classes = updated })
let append_leaf_tree aklass data =
    (* If we assume data is valid and data has aklass's parent then we should be fine *)
    let parent = klass_to_parent aklass in
    if StringMap.mem parent data.classes
        then Left(data)
        else Right(HierarchyIssue("Appending a leaf without a known parent."))
let append_leaf_children aklass data =
    let parent = klass_to_parent aklass in
    let updated = add_map_list parent aklass.klass data.children in
    Left({ data with children = updated })
let append_leaf_parent aklass data =
    let parent = klass_to_parent aklass in
    let updated = StringMap.add aklass.klass parent data.parents in
    Left({ data with parents = updated })
let append_leaf_variables aklass data = match build_var_map aklass with
    | Left(vars) ->
        let updated = StringMap.add aklass.klass vars data.variables in
        Left({ data with variables = updated })
    | Right(collisions) -> Right(DuplicateVariables([(aklass.klass, collisions)]))
let append_leaf_methods aklass data = match build_method_map aklass with
    | Left(meths) ->
        let updated = StringMap.add aklass.klass meths data.methods in
        Left({ data with methods = updated })
    | Right(collisions) -> Right(ConflictingMethods([build_collisions aklass collisions false]))
let append_leaf_refines aklass data = match build_refinement_map aklass with
    | Left(refs) ->
        let updated = StringMap.add aklass.klass refs data.refines in
        Left({ data with refines = updated })
    | Right(collisions) -> Right(ConflictingRefinements([build_collisions aklass collisions true]))
let append_leaf_mains aklass data = match aklass.sections.mains with
    | [] -> Left(data)
    | [main] ->
        let updated = StringMap.add aklass.klass main data.mains in
        Left({ data with mains = updated })
    | _ -> Right(MultipleMains([aklass.klass]))
let append_leaf_ancestor aklass data =
    let parent = klass_to_parent aklass in
    let ancestors = aklass.klass::(StringMap.find parent data.ancestors) in
    let updated = StringMap.add aklass.klass ancestors data.ancestors in
    Left({ data with ancestors = updated })
let append_leaf_distance aklass data =
    let ancestors = StringMap.find aklass.klass data.ancestors in
    let distance = build_distance aklass.klass ancestors in
    let updated = StringMap.add aklass.klass distance data.distance in
    Left({ data with distance = updated })

let append_leaf data aklass =
            Left(data)
    |-> append_leaf_classes aklass
    |-> append_leaf_tree aklass
    |-> append_leaf_children aklass
    |-> append_leaf_parent aklass
    |-> append_leaf_variables aklass
    |-> append_leaf_methods aklass
    |-> append_leaf_refines aklass
    |-> append_leaf_mains aklass
    |-> append_leaf_ancestor aklass
    |-> append_leaf_distance aklass

(**
        Print class data out to stdout.
    *)
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
