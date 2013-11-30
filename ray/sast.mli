open Ast

type env = string StringMap.t

type sexpr = expr * Type.t
type sstmt = stmt * env
