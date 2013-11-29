(* Types *)
type ('a, 'b) either = Left of 'a | Right of 'b

(* Reduce a list of options to the values in the Some constructors *)
let filter_option list =
  let rec do_filter rlist = function
    | [] -> List.rev rlist
    | None::tl -> do_filter rlist tl
    | (Some(v))::tl -> do_filter (v::rlist) tl in
  do_filter [] list

(* Lexically compare two lists of comparable items *)
let rec lexical_compare list1 list2 = match list1, list2 with
  | [], [] -> 0
  | [], _  -> -1
  | _, []  -> 1
  | (x::xs), (y::ys) -> if x < y then -1 else if x > y then 1 else lexical_compare xs ys

(* Loop through a list and find all the items that are minimum with respect to the total
 * ordering cmp. Note can return any size list.
 *)
let find_all_min cmp alist =
  let rec min_find found items = match found, items with
    | _, [] -> List.rev found (* Return in the same order at least *)
    | [], i::is -> min_find [i] is
    | (f::fs), (i::is) -> let result = cmp i f in
      if result = 0 then min_find (i::found) is
      else if result < 0 then min_find [i] is
      else min_find found is in
  min_find [] alist

(* Either monad stuffage *)
let (|>) value func =
  match value with
    | Left(v) -> func(v)
    | Right(problem) -> Right(problem)

