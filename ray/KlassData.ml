open Ast
open Util
open StringModules
open GlobalData
open Klass

(** Build a class_data object. *)

(** Construct an empty class_data object *)
let empty_data : class_data = {
    known = StringSet.empty;
    classes = StringMap.empty;
    parents = StringMap.empty;
    children = StringMap.empty;
    variables = StringMap.empty;
    methods = StringMap.empty;
    refines = StringMap.empty;
    mains = StringMap.empty;
    ancestors = StringMap.empty;
    distance = StringMap.empty;
    dispatcher = StringMap.empty;
    refinable = StringMap.empty;
}

(**
    Map function collisions to the type used for collection that information.
    This lets us have a `standard' form of method / refinement collisions and so
    we can easily build up a list of them.
    @param aklass the class we are currently examining (class name -- string)
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
    (aklass, List.map to_collision funcs)

(** Fold over the values in a class_data record's classes map. *)
let fold_classes data folder init =
    let do_fold _ aklass result = folder result aklass in
    StringMap.fold do_fold data.classes init

(**
    Fold over the values in a class_data record's classes map, but
    enforce building up a StringMap.
  *)
let map_classes data folder = fold_classes data folder StringMap.empty

(**
    Recursively explore the tree starting at the root, accumulating errors
    in a list as we go. The explorer function should take the current class
    the current state, the current errors and return a new state / errors
    pair (updating state when possible if there are errors for further
    accumulation). This is the state that will be passed to all children,
    and the errors will accumulate across all children.
    @param data A class_data record value
    @param explore Something that goes from the current node to a new state/error pair
    @init_state the initial state of the system
    @init_error the initial errors of the system
    @return The final accumulated errors
  *)
let dfs_errors data explore init_state init_error =
    let rec recurse aklass state errors =
        let (state, errors) = explore aklass state errors in
        let explore_kids errors child = recurse child state errors in
        let children = map_lookup_list aklass data.children in
        List.fold_left explore_kids errors children in
    recurse "Object" init_state init_error

(**
    Given a list of classes, build an initial class_data object with
    the known and classes fields set appropriately. If there are any
    duplicate class names a StringSet of the collisions will then be
    returned in Right, otherwise the data will be returned in Left.
    @param klasses A list of classes
    @return Left(data) which is a class_data record with the known
    set filled with names or Right(collisions) which is a set of
    collisions (StringSet.t)
  *)
let initialize_class_data klasses =
    let build_known (set, collisions) aklass =
        if StringSet.mem aklass.klass set
            then (set, StringSet.add aklass.klass collisions)
            else (StringSet.add aklass.klass set, collisions) in
    let klasses = BuiltIns.built_in_classes @ klasses in
    let build_classes map aklass = StringMap.add aklass.klass aklass map in
    let (known, collisions) = List.fold_left build_known (StringSet.empty, StringSet.empty) klasses in
    let classes = List.fold_left build_classes StringMap.empty klasses in
    if StringSet.is_empty collisions
        then Left({ empty_data with known = known; classes = classes })
        else Right(collisions)

(**
    Given an initialized class_data record, build the children map
    from the classes that are stored within it.
    The map is from parent to children list.
    @param data A class_data record
    @return data but with the children.
  *)
let build_children_map data =
    let map_builder map aklass = match aklass.klass with
        | "Object" -> map
        | _ -> add_map_list (klass_to_parent aklass) aklass.klass map in
    let children_map = map_classes data map_builder in
    { data with children = children_map }

(**
    Given an initialized class_Data record, build the parent map
    from the classes that are stored within it.
    The map is from child to parent.
    @param data A class_data record
    @return data but with the parent map updated.
  *)
let build_parent_map data =
    let map_builder map aklass = match aklass.klass with
        | "Object" -> map
        | _ -> StringMap.add (aklass.klass) (klass_to_parent aklass) map in
    let parent_map = map_classes data map_builder in
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
    let folder result aklass = match result with
        | Left(checked) -> from_object aklass.klass checked
        | issue -> issue in
    let checked = StringMap.add "Object" true StringMap.empty in
    match fold_classes data folder (Left(checked)) with
        | Right(issue) -> Some(issue)
        | _ -> None

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
    let folder map aklass = ancestor_builder aklass.klass map in
    let map = StringMap.add "Object" ["Object"] StringMap.empty in
    let ancestor_map = fold_classes data folder map in
    { data with ancestors = ancestor_map }

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
    @return Either a list of collisions (in Right) or the updated record (in Left).
    Collisions are pairs (class name, collisions (var names) for that class)
  *)
let build_class_var_map data =
    let map_builder (klass_map, collision_list) (_, aklass) =
        match build_var_map aklass with
            | Left(var_map) -> (StringMap.add (aklass.klass) var_map klass_map, collision_list)
            | Right(collisions) -> (klass_map, (aklass.klass, collisions)::collision_list) in
    match build_map_track_errors map_builder (StringMap.bindings data.classes) with
        | Left(variable_map) -> Left({ data with variables = variable_map })
        | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(**
    Given a class_data record and a class_def value, return the instance variables (just the
    var_def) that have an unknown type.
    @param data A class_data record value
    @param aklass A class_def value
    @return A list of unknown-typed instance variables in the class
  *)
let type_check_variables data aklass =
    let unknown_type (var_type, _) = not (is_type data var_type) in
    let vars = List.flatten (List.map snd (klass_to_variables aklass)) in
    List.filter unknown_type vars

(**
    Given a class_data record, verify that all instance variables of all classes are of known
    types. Returns the Left of the data if everything is okay, or the Right of a list of pairs,
    first item being a class, second being variables of unknown types (type, name pairs).
    @param data A class_data record value.
    @return Left(data) if everything is okay, otherwise Right(unknown types) where unknown types
    is a list of (class, var_def) pairs.
  *)
let verify_typed data =
    let verify_klass klass_name aklass unknowns = match type_check_variables data aklass with
        | [] -> unknowns
        | bad -> (klass_name, bad)::unknowns in
    match StringMap.fold verify_klass data.classes [] with
        | [] -> Left(data)
        | bad -> Right(bad)

(**
    Given a function, type check the signature (Return, Params).
    @param data A class_data record value.
    @param func An Ast.func_def record
    @return Left(data) if everything is alright; Right([host.]name, option string, (type, name)
    list) if wrong.
  *)
let type_check_func data func =
    let atype = is_type data in
    let check_ret = match func.returns with
        | Some(vtype) -> if atype vtype then None else Some(vtype)
        | _ -> None in
    let check_param (vtype, vname) = if not (atype vtype) then Some((vtype, vname)) else None in
    let bad_params = filter_option (List.map check_param func.formals) in
    match check_ret, bad_params, func.host with
        | None, [], _ -> Left(data)
        | _, _, None -> Right((func.name, check_ret, bad_params))
        | _, _, Some(host) -> Right((host ^ "." ^ func.name, check_ret, bad_params))

(**
    Given a class_data object and a klass, verify that all of its methods have good types
    (Return and parameters).
    @param data A class_data record object
    @param aklass A class_def object
    @return Left(data) if everything went okay; Right((klass name, (func name, option string,
    (type, name) list) list))
  *)
let type_check_class data aklass =
    let folder bad func = match type_check_func data func with
        | Left(data) -> bad
        | Right(issue) -> issue::bad in
    let funcs = List.flatten (List.map snd (klass_to_functions aklass)) in
    match List.fold_left folder [] funcs with
        | [] -> Left(data)
        | bad -> Right((aklass.klass, bad))

(**
    Given a class_data object, verify that all classes have methods with good signatures
    (Return and parameters)
    @param data A class_data record object
    @param aklass A class_def object
    @return Left(data) if everything went okay; Right((klass name, bad_sig list) list)
    where bad_sig is (func_name, string option, (type, var) list))
  *)
let type_check_signatures data =
    let folder klass_name aklass bad = match type_check_class data aklass with
        | Left(data) -> bad
        | Right(issue) -> issue::bad in
    match StringMap.fold folder data.classes [] with
        | [] -> Left(data)
        | bad -> Right(bad)

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
    @return Either a list of collisions (in Right) or the updated record (in Left).
    Collisions are pairs (class name, colliding methods for that class). Methods collide
    if they have conflicting signatures (ignoring return type).
  *)
let build_class_method_map data =
    let map_builder (klass_map, collision_list) (_, aklass) =
        match build_method_map aklass with
            | Left(method_map) -> (StringMap.add aklass.klass method_map klass_map, collision_list)
            | Right(collisions) -> (klass_map, (build_collisions aklass.klass collisions false)::collision_list) in
    match build_map_track_errors map_builder (StringMap.bindings data.classes) with
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
let build_class_refinement_map data =
    let map_builder (klass_map, collision_list) (_, aklass) =
        match build_refinement_map aklass with
            | Left(refinement_map) -> (StringMap.add aklass.klass refinement_map klass_map, collision_list)
            | Right(collisions) -> (klass_map, (build_collisions aklass.klass collisions true)::collision_list) in
    match build_map_track_errors map_builder (StringMap.bindings data.classes) with
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
let build_main_map data =
    let add_klass (map, collisions) (_, aklass) = match aklass.sections.mains with
        | [] -> (map, collisions)
        | [main] -> (StringMap.add aklass.klass main map, collisions)
        | _ -> (map, aklass.klass :: collisions) in
    match build_map_track_errors add_klass (StringMap.bindings data.classes) with
        | Left(main_map) -> Left({ data with mains = main_map })
        | Right(collisions) -> Right(collisions) (* Same value different types parametrically *)

(**
    Given a class_data record, verify that there are no double declarations of instance
    variables as you go up the tree. This means that no two classes along the same root
    leaf path can have the same public / protected variables, and a private cannot be
    a public/protected variable of an ancestor.
    @param data A class_data record.
    @return Left(data) if everything was okay or Right(collisions) where collisions is
    a list of pairs of collision information - first item class, second item a list of
    colliding variables for that class (name, ancestor where they collide)
  *)
let check_field_collisions data =
    let check_vars aklass var (section, _) (fields, collisions) = match map_lookup var fields, section with
        | Some(ancestor), _ -> (fields, (ancestor, var)::collisions)
        | None, Privates -> (fields, collisions)
        | None, _ -> (StringMap.add var aklass fields, collisions) in

    let check_class_vars aklass fields =
        let vars = StringMap.find aklass data.variables in
        StringMap.fold (check_vars aklass) vars (fields, []) in

    let dfs_explorer aklass fields collisions =
        match check_class_vars aklass fields with
            | (fields, []) -> (fields, collisions)
            | (fields, cols) -> (fields, (aklass, cols)::collisions) in

    match dfs_errors data dfs_explorer StringMap.empty [] with
        | [] -> Left(data)
        | collisions -> Right(collisions)

(**
    Check to make sure that we don't have conflicting signatures as we go down the class tree.
    @param data A class_data record value
    @return Left(data) if everything is okay, otherwise a list of (string
  *)
let check_ancestor_signatures data =
    let check_sigs meth_name funcs (methods, collisions) =
        let updater (known, collisions) func =
            if List.exists (conflicting_signatures func) known
                then (known, func::collisions)
                else (func::known, collisions) in
        let apriori = map_lookup_list meth_name methods in
        let (known, collisions) = List.fold_left updater (apriori, collisions) funcs in
        (StringMap.add meth_name known methods, collisions) in

    let skip_init meth_name funcs acc = match meth_name with
        | "init" -> acc
        | _ -> check_sigs meth_name funcs acc in

    let check_class_meths aklass parent_methods =
        let methods = StringMap.find aklass data.methods in
        StringMap.fold skip_init methods (parent_methods, []) in

    let dfs_explorer aklass methods collisions =
       match check_class_meths aklass methods with
           | (methods, []) -> (methods, collisions)
           | (methods, cols) -> (methods, (build_collisions aklass cols false)::collisions) in

    match dfs_errors data dfs_explorer StringMap.empty [] with
        | [] -> Left(data)
        | collisions -> Right(collisions)

(**
    Verifies that each class is able to be instantiated.
    @param data A class_data record
    @return Either the data is returned in Left or a list of uninstantiable classes in Right
  *)
let verify_instantiable data =
    let uninstantiable klass =
        let inits = class_method_lookup data klass "init" in
        not (List.exists (fun func -> func.section <> Privates) inits) in
    let klasses = StringSet.elements data.known in
    match List.filter uninstantiable klasses with
        | [] -> Left(data)
        | bad -> Right(bad)

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
    Update the refinement dispatch uid table with a given set of refinements.
    @param parent The class the refinements will come from
    @param refines A list of refinements
    @param table The refinement dispatch table
    @return The updated table
  *)
let update_dispatch parent refines table =
    let folder amap f = StringMap.add f (UID.uid_counter ()) amap in
    let toname f = match f.host with
        | Some(host) -> host ^ "." ^ f.name
        | _ -> raise(Invalid_argument("Compiler error; we have refinement without host for " ^ f.name ^ " in " ^ f.inklass ^ ".")) in

    let map = if StringMap.mem parent table then StringMap.find parent table else StringMap.empty in
    let unknown f = not (StringMap.mem f map) in
    let names = List.filter unknown (List.map toname refines) in
    let map = List.fold_left folder map names in
    StringMap.add parent map table

(**
    Add a class (class name - string) -> full method name (host.name - string) -> uid (string)
    table so that dispatching can be done later on.
    @param data A class_data record.
    @return The class_data record with the dispatching identifier added.
  *)
let build_dispatch_map data =
    let updater klass_name aklass table = match klass_name with
        | "Object" -> table (* Object has no refinements *)
        | _ -> let parent = klass_to_parent aklass in update_dispatch parent aklass.sections.refines table in
    let dispatcher = StringMap.fold updater data.classes StringMap.empty in
    { data with dispatcher = dispatcher }

(**
    Update the refinement dispatch uid table with a given set of refinements.
    @param parent The class the refinements will come from
    @param refines A list of refinements
    @param table The refinement dispatch table
    @return The updated table
  *)
let update_refinable parent refines table =
    let toname f = match f.host with
        | Some(host) -> host
        | _ -> raise(Invalid_argument("Compiler error; we have refinement without host for " ^ f.name ^ " in " ^ f.inklass ^ ".")) in
    let folder amap f = add_map_list (toname f) f amap in
    let map = if StringMap.mem parent table then StringMap.find parent table else StringMap.empty in
    let map = List.fold_left folder map refines in
    StringMap.add parent map table

(**
    Add the refinable (class name -> host.name -> refinables list) table to the
    given class_data record, returning the updated record.
    @param data A class_data record info
    @return A class_data object with the refinable updated
  *)
let build_refinable_map data =
    let updater klass_name aklass table = match klass_name with
        | "Object" -> table
        | _ -> let parent = klass_to_parent aklass in update_refinable parent aklass.sections.refines table in
    let refinable = StringMap.fold updater data.classes StringMap.empty in
    { data with refinable = refinable}

(** These are just things to pipe together building a class_data record pipeline *)
let initial_data klasses = match initialize_class_data klasses with
    | Left(data) -> Left(data)
    | Right(collisions) -> Right(DuplicateClasses(StringSet.elements collisions))
let append_children data = Left(build_children_map data)
let append_parent data = Left(build_parent_map data)
let test_tree data = match is_tree_hierarchy data with
    | None -> Left(data)
    | Some(problem) -> Right(HierarchyIssue(problem))
let append_ancestor data = Left(build_ancestor_map data)
let append_distance data = Left(build_distance_map data)
let append_variables data = match build_class_var_map data with
    | Left(data) -> Left(data)
    | Right(collisions) -> Right(DuplicateVariables(collisions))
let test_types data = match verify_typed data with
    | Left(data) -> Left(data)
    | Right(bad) -> Right(UnknownTypes(bad))
let test_fields data = match check_field_collisions data with
    | Left(data) -> Left(data)
    | Right(collisions) -> Right(DuplicateFields(collisions))
let append_methods data = match build_class_method_map data with
    | Left(data) -> Left(data)
    | Right(collisions) -> Right(ConflictingMethods(collisions))
let test_init data = match verify_instantiable data with
    | Left(data) -> Left(data)
    | Right(bad) -> Right(Uninstantiable(bad))
let test_inherited_methods data = match check_ancestor_signatures data with
    | Left(data) -> Left(data)
    | Right(collisions) -> Right(ConflictingInherited(collisions))
let append_refines data = match build_class_refinement_map data with
    | Left(data) -> Left(data)
    | Right(collisions) -> Right(ConflictingRefinements(collisions))
let test_signatures data = match type_check_signatures data with
    | Left(data) -> Left(data)
    | Right(bad) -> Right(PoorlyTypedSigs(bad))
let append_dispatcher data = Left(build_dispatch_map data)
let append_refinable data = Left(build_refinable_map data)
let append_mains data = match build_main_map data with
    | Left(data) -> Left(data)
    | Right(collisions) -> Right(MultipleMains(collisions))

let build_class_data klasses = seq (initial_data klasses)
    [ append_children ; append_parent ; test_tree ; append_ancestor ;
      append_distance ; append_variables ; test_fields ; append_methods ;
      test_init ; append_refines ; append_mains ]

let build_class_data_test klasses = seq (initial_data klasses)
    [ append_children ; append_parent ; test_tree ; append_ancestor ;
      append_distance ; append_variables ; test_fields ; test_types ;
      append_methods ; test_init ; test_inherited_methods ; append_refines ;
      test_signatures ; append_dispatcher ; append_refinable ; append_mains ]

let append_leaf_known aklass data =
    let updated = StringSet.add aklass.klass data.known in
    if StringSet.mem aklass.klass data.known
        then Right(DuplicateClasses([aklass.klass]))
        else Left({ data with known = updated })
let append_leaf_classes aklass data =
    let updated = StringMap.add aklass.klass aklass data.classes in
    Left({ data with classes = updated })
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
let append_leaf_test_fields aklass data =
    let folder collisions var = match class_field_lookup data (klass_to_parent aklass) var with
        | Some((_, _, Privates)) -> collisions
        | Some((ancestor, _, section)) -> (ancestor, var)::collisions
        | _ -> collisions in
    let variables = List.flatten (List.map snd (klass_to_variables aklass)) in
    let varnames = List.map snd variables in
    match List.fold_left folder [] varnames with
        | [] -> Left(data)
        | collisions -> Right(DuplicateFields([(aklass.klass, collisions)]))
let append_leaf_type_vars aklass data =
    match type_check_variables data aklass with
        | [] -> Left(data)
        | bad -> Right(UnknownTypes([(aklass.klass, bad)]))
let append_leaf_methods aklass data = match build_method_map aklass with
    | Left(meths) ->
        let updated = StringMap.add aklass.klass meths data.methods in
        Left({ data with methods = updated })
    | Right(collisions) -> Right(ConflictingMethods([build_collisions aklass.klass collisions false]))
let append_leaf_test_inherited aklass data =
    let folder collisions meth = match class_ancestor_method_lookup data aklass.klass meth.name true with
        | [] -> collisions
        | funcs -> match List.filter (conflicting_signatures meth) funcs with
            | [] -> collisions
            | cols -> cols in
    let functions = List.flatten (List.map snd (klass_to_methods aklass)) in
    match List.fold_left folder [] functions with
        | [] -> Left(data)
        | collisions -> Right(ConflictingInherited([build_collisions aklass.klass collisions false]))
let append_leaf_instantiable aklass data =
    let is_init mem = match mem with
        | InitMem(_) -> true
        | _ -> false in
    if List.exists is_init (aklass.sections.protects) then Left(data)
    else if List.exists is_init (aklass.sections.publics) then Left(data)
    else Right(Uninstantiable([aklass.klass]))
let append_leaf_refines aklass data = match build_refinement_map aklass with
    | Left(refs) ->
        let updated = StringMap.add aklass.klass refs data.refines in
        Left({ data with refines = updated })
    | Right(collisions) -> Right(ConflictingRefinements([build_collisions aklass.klass collisions true]))
let append_leaf_mains aklass data = match aklass.sections.mains with
    | [] -> Left(data)
    | [main] ->
        let updated = StringMap.add aklass.klass main data.mains in
        Left({ data with mains = updated })
    | _ -> Right(MultipleMains([aklass.klass]))
let append_leaf_signatures aklass data = match type_check_class data aklass with
    | Left(data) -> Left(data)
    | Right(bad) -> Right(PoorlyTypedSigs([bad]))
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
let append_leaf_dispatch aklass data =
    let parent = klass_to_parent aklass in
    let updated = update_dispatch parent aklass.sections.refines data.dispatcher in
    Left({ data with dispatcher = updated })
let append_leaf_refinable aklass data =
    let parent = klass_to_parent aklass in
    let updated = update_refinable parent aklass.sections.refines data.refinable in
    Left({ data with refinable = updated })

let append_leaf data aklass =
    let with_klass f = f aklass in
    let actions =
        [ append_leaf_known ; append_leaf_classes ; append_leaf_children ; append_leaf_parent ;
          append_leaf_ancestor ; append_leaf_distance ; append_leaf_variables ; append_leaf_test_fields ;
          append_leaf_methods ; append_leaf_instantiable ; append_leaf_refines ; append_leaf_signatures ;
          append_leaf_mains ] in
    seq (Left(data)) (List.map with_klass actions)

let append_leaf_test data aklass =
    let with_klass f = f aklass in
    let actions =
        [ append_leaf_known ; append_leaf_classes ; append_leaf_children ; append_leaf_parent ;
          append_leaf_ancestor ; append_leaf_distance ; append_leaf_variables ; append_leaf_test_fields ;
          append_leaf_type_vars ; append_leaf_methods ; append_leaf_instantiable ; append_leaf_test_inherited ;
          append_leaf_refines ; append_leaf_dispatch ; append_leaf_refinable ; append_leaf_mains ] in
    seq (Left(data)) (List.map with_klass actions)

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

    let func_of_list funcs =
        let sigs = List.map (fun f -> "\n\t\t" ^ f.inklass ^ "->" ^ (full_signature_string f)) funcs in
        String.concat "" sigs in

    let class_printer cdef =
        let rec count sect = function
            | (where, members)::_ when where = sect -> List.length members
            | _::rest -> count sect rest
            | [] -> raise(Failure("The impossible happened -- searching for a section that should exist doesn't exist.")) in
        let vars = klass_to_variables cdef in
        let funcs = klass_to_functions cdef in
        let format = "class %s extends %s and has\n" ^^
                                  "\t\t(%d/%d/%d) methods -- private, protected, public (non-inherited)\n" ^^
                                  "\t\t(%d/%d/%d) fields -- private, protected, public (non-inherited)\n" ^^
                                  "\t\t%d refinements, %d mains" in
        let parent = match cdef.klass with
            | "Object" -> "----"
            | _ -> klass_to_parent cdef in
        Format.sprintf format cdef.klass parent
            (count Privates funcs) (count Protects funcs) (count Publics funcs)
            (count Refines funcs) (count Mains funcs)
            (count Privates vars) (count Protects vars) (count Publics vars) in

    let print_list list =
        let rec list_printer spaces endl space = function
            | [] -> if endl then () else print_newline ()
            | list when spaces = 0 -> print_string "\t"; list_printer 8 false false list
            | list when spaces > 60 -> print_newline (); list_printer 0 true false list
            | item::rest ->
              if space then print_string " " else ();
              print_string item;
              list_printer (spaces + String.length item) false true rest in
        list_printer 0 true false list in

    Printf.printf "Types:\n";
    print_list (StringSet.elements data.known);
    print_newline ();
    map_printer data.classes "Classes" class_printer;
    print_newline ();
    map_printer data.parents "Parents" id;
    print_newline ();
    map_printer data.children "Children" from_list;
    print_newline ();
    map_printer data.ancestors "Ancestors" from_list;
    print_newline ();
    table_printer data.distance "Distance" string_of_int;
    print_newline ();
    table_printer data.variables "Variables" (fun (sect, t) -> Format.sprintf "%s %s" (section_string sect) t);
    print_newline ();
    table_printer data.methods "Methods" func_list;
    print_newline ();
    table_printer data.refines "Refines" func_list;
    print_newline ();
    map_printer data.mains "Mains" full_signature_string;
    print_newline ();
    table_printer data.dispatcher "Dispatch" id;
    print_newline ();
    table_printer data.refinable "Refinable" func_of_list;
