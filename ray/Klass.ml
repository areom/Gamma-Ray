open Ast
open Util
open StringModules
open GlobalData

(** Approximates a class *)
(**
    From a class get the parent
    @param aklass is a class_def to get the parent of
    @return The name of the parent object
  *)
let klass_to_parent aklass = match aklass with
    | { klass = "Object" } -> raise(Invalid_argument("Cannot get parent of the root"))
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
    is a list of the variables defs (type, name). Note that this only
    returns pairs for Publics, Protects, and Privates as the others
    cannot have variables
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
    is a list of the methods. Note that this only returns the methods
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
    Given a class_data record, a class_name, and a variable name, lookup the class in the hierarchy
    that provides access to that variable from within that class (i.e. private in that class or
    public / protected in an ancestor).
    @param data A class_data record.
    @param klass_name The name of a class (string)
    @param var_name The name of a variable (string).
    @return (class (string), type (string), class_section) option (None if not found).
  *)
let class_field_lookup data klass_name var_name =
    let var_lookup klass = class_var_lookup data klass var_name in
    let rec lookup klass sections = match var_lookup klass, klass with
        | Some((sect, vtype)), _ when List.mem sect sections -> Some((klass, vtype, sect))
        | _, "Object" -> None
        | _, _ -> lookup (StringMap.find klass data.parents) [Publics; Protects] in
    lookup klass_name [Publics; Protects; Privates]

(**
    Given a class_data record, a class name, a var_name, and whether the receiver of the field lookup
    is this, return the lookup of the field in the ancestry of the object. Note that this restricts
    things that should be kept protected (thus this thusly passed)
    @param data A class_data record
    @param klass_name The name of a class (string)
    @param var_name The name of a variable (string)
    @return Either the left of a triple (class found, type, section) or a Right of a boolean, which
    is true if the item was found but inaccessible and false otherwise.
  *)
let class_field_far_lookup data klass_name var_name this =
    match class_field_lookup data klass_name var_name with
        | Some((klass, vtyp, section)) when this || section = Publics -> Left((klass, vtyp, section))
        | Some(_) -> Right(true)
        | None -> Right(false)

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
    Given a class_data record, a class name, a method name, and whether the current context is
    `this' (i.e. if we want private / protected / etc), then return all methods in the ancestry
    of that class with that name (in the appropriate sections).
    @param data A class_data record value
    @param klass_name The name of a class.
    @param method_name The name of a method to look up
    @param this search mode -- true means public/protected/private and then public/protected,
    false is always public
    @return A list of methods with the given name.
  *)
let class_ancestor_method_lookup data klass_name method_name this =
    let (startsects, recsects) = if this then ([Publics; Protects; Privates], [Publics; Protects]) else ([Publics], [Publics]) in
    let rec find_methods found aklass sects =
        let accessible f = List.mem f.section sects in
        let funcs = List.filter accessible (class_method_lookup data aklass method_name) in
        let found = funcs @ found in
        if aklass = "Object" then found
        else if method_name = "init" then found
        else find_methods found (StringMap.find aklass data.parents) recsects in
    find_methods [] klass_name startsects

(**
    Given a class_data record, class name, method name, and refinement name, return the list of
    refinements in that class for that method with that name.
    @param data A class_data record value
    @param klass_name A class name
    @param method_name A method name
    @param refinement_name A refinement name
    @return A list of func_def values that match the given requirements. Note that this returns the
    functions defined IN class name, not the ones that could be used INSIDE class name (via a refine
    invocation). i.e. functions that may be invoked by the parent.
  *)
let refine_lookup data klass_name method_name refinement_name =
    match map_lookup klass_name data.refines with
        | Some(map) -> map_lookup_list (method_name ^ "." ^ refinement_name) map
        | _ -> []

(**
    Given a class_data record, a class name, a method name, and a refinement name, return the list
    of refinements across all subclasses for the method with that name.
    @param data A class_data record value
    @param klass_name A class name
    @param method_name A method name
    @param refinement_name A refinement name
    @return A list of func_def values that meet the criteria and may be invoked by this given method.
    i.e. these are all functions residing in SUBCLASSES of the named class.
  *)
let refinable_lookup data klass_name method_name refinement_name =
    let refines = match map_lookup klass_name data.refinable with
        | Some(map) -> map_lookup_list method_name map
        | None -> [] in
    List.filter (fun f -> f.name = refinement_name) refines

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
let is_type data atype =
    let lookup = try String.sub atype 0 (String.index atype '[') with
        | Not_found -> atype in
    StringSet.mem lookup data.known

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
    Return whether a function's return type is compatible with a desired return type.
    Note that if the desired return type is None then the function is compatible.
    Otherwise if it is not None and the function's is, then it is not compatible.
    Lastly, if the desired type is a supertype of the function's return type then the
    function is compatible.
    @param data A class_data record value
    @param ret_type The desired return type
    @param func A func_def to test.
    @return True if compatible, false if not.
  *)
let compatible_return data ret_type func =
    match ret_type, func.returns with
        | None, _ -> true
        | _, None -> false
        | Some(desired), Some(given) -> is_subtype data given desired

(**
    Return whether a function's signature is completely compatible with a return type
    and a set of actuals
    @param data A class_data record value
    @param ret_type The return type (string option)
    @param actuals The list of actual types
    @param func A func_def value
    @return True if compatible, false if not.
  *)
let compatible_signature data ret_type actuals func =
    compatible_return data ret_type func && compatible_function data actuals func

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
        | _ -> raise(Invalid_argument("Multiple methods named " ^ method_name ^ " of the same signature in " ^ klass_name ^ "; Compiler error."))

let best_inherited_method data klass_name method_name actuals this =
    let methods = class_ancestor_method_lookup data klass_name method_name this in
    match best_matching_signature data actuals methods with
        | [] -> None
        | [func] -> Some(func)
        | _ -> raise(Invalid_argument("Multiple methods named " ^ method_name ^ " of the same signature inherited in " ^ klass_name ^ "; Compiler error."))

(**
    Given the name of a refinement to apply, the list of actual types,
    find the compatible refinements via the data / klass_name / method_name.
    Partition the refinements by their inklass value and then return a list
    of the best matches from each partition.
    @param data A class_data record value
    @param klass_name A class name
    @param method_name A method name
    @param refine_name A refinement name
    @param actuals The types of the actual arguments
    @return A list of functions to switch on based on the actuals.
  *)
let refine_on data klass_name method_name refine_name actuals ret_type =
    (* These are all the refinements available from subclasses *)
    let refines = refinable_lookup data klass_name method_name refine_name in

    (* Compatible functions *)
    let compat = List.filter (compatible_signature data ret_type actuals) refines in

    (* Organize by inklass *)
    let to_class map f = add_map_list f.inklass f map in
    let by_class = List.fold_left to_class StringMap.empty compat in

    (* Now make a map of only the best *)
    let best funcs = match best_matching_signature data actuals funcs with
        | [func] -> func
        | _ -> raise(Failure("Compiler error finding a unique best refinement.")) in
    let to_best klass funcs map = StringMap.add klass (best funcs) map in
    let best_map = StringMap.fold to_best by_class StringMap.empty in

    (* Now just return the bindings from the best *)
    List.map snd (StringMap.bindings best_map)

(**
    Get the names of the classes in level order (i.e. from root down).
    @param data A class_data record
    @return The list of known classes, from the root down.
  *)
let get_class_names data =
    let kids aklass = map_lookup_list aklass data.children in
    let rec append found = function
        | [] -> List.rev found
        | items -> let next = List.flatten (List.map kids items) in
            append (items@found) next in
    append [] ["Object"]


(**
    Get leaf classes
    @param data A class_data record
    @return A list of leaf classes
  *)
let get_leaves data =
    let is_leaf f = match map_lookup_list f data.children with
        | [] -> true
        | _ -> false in
    let leaves = StringSet.filter is_leaf data.known in
    StringSet.elements leaves

