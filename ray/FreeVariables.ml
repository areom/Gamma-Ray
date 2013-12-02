open Parser
open Ast
open Util
module StringSet = Set.Make(String)

let free_vars bound stmts =
  let rec get_free_vars free = function
    | [] -> free
    | (bound, Left(stmts))::todo -> get_free_stmts free bound todo stmts
    | (bound, Right(exprs))::todo -> get_free_exprs free bound todo exprs
  and get_free_stmts free bound todo = function
    | [] -> get_free_vars free todo
    | stmt::rest ->
      let (expr_block_list, stmt_block_list, decl) = match stmt with
        | Decl(((_, var), e)) -> ([option_as_list e], [], Some(var))
        | Expr(e)             -> ([[e]], [], None)
        | Return(e)           -> ([option_as_list e], [], None)
        | Super(es)           -> ([es], [], None)
        | While(e, body)      -> ([[e]], [body], None)
        | If(parts)           -> let (es, ts) = List.split parts in
                                 ([filter_option es], ts, None) in
      let expressions = List.map (function exprs -> (bound, Right(exprs))) expr_block_list in
      let statements  = List.map (function stmts -> (bound, Left(stmts))) stmt_block_list in
      let bound = match decl with
        | Some(var) -> StringSet.add var bound
        | _ -> bound in
      get_free_stmts free bound (expressions @ statements @ todo) rest
  and get_free_exprs free bound todo = function
    | [] -> get_free_vars free todo
    | expr::rest ->
      let (exprs, blocks, id) = match expr with
        | NewObj(_, args)           -> (args, [], None)
        | Assign(l, r)              -> ([l; r], [], None)
        | Deref(v, i)               -> ([v; i], [], None)
        | Field(e, _)               -> ([e], [], None)
        | Invoc(e, _, args)         -> (e::args, [], None)
        | Unop(_, e)                -> ([e], [], None)
        | Binop(l, _, r)            -> ([l; r], [], None)
        | Refine(_, args, _)        -> (args, [], None)
        | This                      -> ([], [], None)
        | Null                      -> ([], [], None)
        | Refinable(_)              -> ([], [], None)
        | Literal(_)                -> ([], [], None) 
        | Id(id)                    -> ([], [], decide_option id (not (StringSet.mem id bound)))
        | Anonymous(_, args, funcs) -> let bodies = List.map (fun f -> f.body) funcs in
                                       (args, bodies, None) in
      let rest = exprs @ rest in
      let todo = (List.map (fun body -> (bound, Left(body))) blocks) @ todo in
      let free = match id with
        | Some(id) -> StringSet.add id free
        | None -> free in
      get_free_exprs free bound todo rest in

  get_free_vars StringSet.empty [(bound, Left(stmts))]
