open Ast

(**
    Various utility functions
*)

(* Types *)
(**
	Paramaterized variable typing for building binary ASTs
	@see <http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora016.html#toc19> For more details on paramterized typing
*)
type ('a, 'b) either = Left of 'a | Right of 'b

(**
    Reduce a list of options to the values in the Some constructors
*)
let filter_option list =
  let rec do_filter rlist = function
    | [] -> List.rev rlist
    | None::tl -> do_filter rlist tl
    | (Some(v))::tl -> do_filter (v::rlist) tl in
  do_filter [] list

let option_as_list = function
  | Some(v) -> [v]
  | _ -> []

let decide_option x = function
  | true -> Some(x)
  | _ -> None

(** Lexically compare two lists of comparable items *)
let rec lexical_compare list1 list2 = match list1, list2 with
  | [], [] -> 0
  | [], _  -> -1
  | _, []  -> 1
  | (x::xs), (y::ys) -> if x < y then -1 else if x > y then 1 else lexical_compare xs ys

(**
    Loop through a list and find all the items that are minimum with respect to the total
    ordering cmp. (If an item is found to be a minimum, any item that is found to
    be equal to the item is in the returned list.) Note can return any size list.
    @param cmp A comparator function
    @param alist A list of items
    @return A list of one or more items deemed to be the minimum by cmp.
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

(**
    Either monad stuffage
    @param value A monad
    @param func A function to run on a monad
    @return The result of func if we're on the left side, or the next part of the tree if we're on the right
*)
let (|>) value func =
  match value with
    | Left(v) -> func(v)
    | Right(problem) -> Right(problem)

(**
    Return the length of a block -- i.e. the total number of statements (recursively) in it
    @param stmt_list A list of stmt type objects
    @return An int encoding the length of a block    
*)
let get_statement_count stmt_list =
  let rec do_count stmts blocks counts = match stmts, blocks with
    | [], [] -> counts
    | [], _  -> do_count blocks [] counts
    | (stmt::rest), _ -> match stmt with
      | Decl(_) -> do_count rest blocks (counts + 1)
      | Expr(_) -> do_count rest blocks (counts + 1)
      | Return(_) -> do_count rest blocks (counts + 1)
      | Super(_) -> do_count rest blocks (counts + 1)
      | While(_, block) -> do_count rest (block @ blocks) (counts + 1)
      | If(parts) ->
        let ifblocks = List.map snd parts in
        let ifstmts = List.flatten ifblocks in
        do_count rest (ifstmts @ blocks) (counts + 1) in
  do_count stmt_list [] 0

