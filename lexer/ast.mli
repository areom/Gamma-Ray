type lit =
    Int of int
  | Float of float
  | String of string
  | Bool of bool

type arith = Add | Sub | Prod | Quot | Div | Mod | Neg | Pow
type numtest = Eq | Neq | Less | Grtr | Leq | Geq
type combtest = And | Or | Nand | Nor | Xor | Not
type op = Arithmetic of arith | NumTest of numtest | CombTest of combtest

type expr =
    Id of string
  | Literal of lit
  | Invoc of expr * expr * expr list (* object.function (args), args and object can be Noexpr*)
  | Field of expr * expr (* road.pavement *)
  | Deref of expr * expr (* road[pavement] *)
  | Unop of op * expr (* !x *)
  | Binop of expr * op * expr (* x + y *)
  | Noexpr

type var_def = string * string (* Oh typing, you pain in the ass *)

type stmt =
    Decl of var_def
  | Assign of string * expr (* as our grammer is written, assignment is a statement. Do we want this? *)
  | If of (expr option * stmt list) list
  | While of expr * stmt list
  | Expr of expr
  | Return of expr

(* we have four different kinds of callable code blocks:
 *  main: only has formals, body (name / static / host known)
 *  init: only has formals, body (name / static / host known)
 *  refine: has host, name, formals, body (static known)
 *  method: has name, formals, body (host / static known)
 *)
type func_def = {
  returns : string option;
  host    : string option;
  name    : string;
  static  : bool;
  formals : var_def list;
  body    : stmt list;
}

(* A member is either a variable or some sort of function *)
type member_def = VarMem of var_def | MethodMem of func_def | InitMem of func_def

(* Things that can go in a class *)
type class_sections_def = {
  privates : member_def list;
  protects : member_def list;
  publics  : member_def list;
  refines  : func_def list;
  mains    : func_def list;
}
	
(* Just pop init and main in there? *)
type class_def = {
  klass    : string;
  parent   : string option;
  sections : class_sections_def;
}
