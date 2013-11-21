open Ast
module StringMap = Map.Make (String);;

(* Class inspection functions *)
let klass_to_parent = function
  | { parent = None; _ } -> "Object"
  | { parent = Some(aklass); _ } -> aklass

let klass_to_sections aklass =
  let s = aklass.sections in [(Public, s.publics), (Protected, s.protects), (Private, s.privates)]

(* make a map children map
 *   parent (name -- string) -> children (names -- string) list
 *)
let build_children_map klasses =
  let map_builder map aklass =
    let parent = klass_to_parent aklass in
    let child  = aklass.klass in
    if StringMap.mem parent map then
      let children = StringMap.find parent map in
      StringMap.add parent (child::children) map
    else
      StringMap.add parent [child] map in
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

(* For a given class, build a map of variable names to variable information
 *   var name -> (access mode, type)
 *)
let build_var_map aklass =
  let add_var access map = function
    | VarDef((typeId, varId)) -> StringMap.add varId (access, typeId) map
    | _ -> map in
  let map_builder (access, section) map = List.fold_left (add_var access) map section in
  List.fold_left map_builder StringMap.empty (klass_to_sections aklass)

(* Build up a map from class to variable map (i.e. to the above)
 *   class name -> (var name -> (access mode, type)) map
 *)
let build_class_var_map klasses =
  let map_builder map aklass = StringMap.add (aklass.klass) (build_var_map aklass) map in
  List.fold_left map_builder StringMap.empty klasses

(* Given a class -> var map table as above, do a lookup -- returns option *)
let class_var_lookup map klassName varName =
  if StringMap.mem klassName map then
    let varTable = StringMap.find klassName map in
    if StringMap.mem varName varTable then Some(StringMap.find varName varTable) else None
  else None
