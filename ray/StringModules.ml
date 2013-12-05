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
