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
    This
  | Null
  | Id of string
  | NewObj of string * expr list
  | Literal of lit
  | Assign of expr * expr  (* memory := data -- whether memory is good is a semantic issue *)
  | Deref of expr * expr (* road[pavement] *)
  | Field of expr * string (* road.pavement *)
  | Invoc of expr * string * expr list (* receiver.method(args) *)
  | Unop of op * expr (* !x *)
  | Binop of expr * op * expr (* x + y *)
  | Refine of string * expr list * string
  | Refinable of string (* refinable *)

type var_def = string * string  (* Oh typing, you pain in the ass, add a int for array *)

type stmt =
    Decl of var_def * expr option
  | If of (expr option * stmt list) list
  | Assign of string * expr
  | While of expr * stmt list
  | Expr of expr
  | Return of expr
  | Super of expr list

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
