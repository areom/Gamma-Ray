open Util

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

(**
  Convenience type to make reading table types easier. A lookup_table
  is a primary key -> second key -> value map (i.e. the values of the
  first StringMap are themselves StringMap maps...
*)
type 'a lookup_table = 'a StringMap.t StringMap.t

(**
  Convenience type to make reading string maps easier. A lookup_map
  is just a StringMap map.
*)
type 'a lookup_map = 'a StringMap.t


(** Print the contents of a lookup_map *)
let print_lookup_map map stringer =
  let print_item (secondary, item) =
    print_string (stringer secondary item) in
  List.iter print_item (StringMap.bindings map)

(** Print the contents of a lookup_table *)
let print_lookup_table table stringer =
  let print_lookup_map (primary, table) =
    print_lookup_map table (stringer primary) in
  List.iter print_lookup_map (StringMap.bindings table)


(**
    To put it into symbols, we have builder : (StringMap, errorList) -> item -> (StringMap', errorList')
    @param builder A function that accepts a StringMap/(error list) pair and a new item
    and returns a new pair with either and updated map or updated error list
    @param alist The list of data to build the map out of.
*)
let build_map_track_errors builder alist =
  match List.fold_left builder (StringMap.empty, []) alist with
    | (value, []) -> Left(value)
    | (_, errors) -> Right(errors)

(**
    Look a value up in a map
    @param key The key to look up
    @param map The map to search in
    @return Some(value) or None
*)
let map_lookup key map = if StringMap.mem key map
  then Some(StringMap.find key map)
  else None

(**
    Look a list up in a map
    @param key The key to look up
    @param map The map to search in
    @return a list or None
*)
let map_lookup_list key map = if StringMap.mem key map
  then StringMap.find key map
  else []

(** Updating a string map that has list of possible values *)
let add_map_list key value map =
  if StringMap.mem key map
    then StringMap.add key (value::(StringMap.find key map)) map
    else StringMap.add key [value] map

(** Update a map but keep track of collisions *)
let add_map_unique key value (map, collisions) =
  if StringMap.mem key map
    then (map, key::collisions)
    else (StringMap.add key value map, collisions)
