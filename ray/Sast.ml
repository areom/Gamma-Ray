type t = Int | Float | Bool | String

type varkind = Instance | Local


type environment = (string * varkind) Map.Make(String).t


type lit =
    Int of int
  | Float of float
  | String of string
  | Bool of bool

type arith = Add | Sub | Prod | Quot | Div | Mod | Neg | Pow
type numtest = Eq | Neq | Less | Grtr | Leq | Geq
type combtest = And | Or | Nand | Nor | Xor | Not
type op = Arithmetic of arith | NumTest of numtest | CombTest of combtest

type (*expr =
    This
  | Null
  | Id of string
  | NewObj of string * expr list
  | Anonymous of string * expr list * func_def list
  | Literal of lit
  | Assign of expr * expr  (* memory := data -- whether memory is good is a semantic issue *)
  | Deref of expr * expr (* road[pavement] *)
  | Field of expr * string (* road.pavement *)
  | Invoc of expr * string * expr list (* receiver.method(args) *)
  | Unop of op * expr (* !x *)
  | Binop of expr * op * expr (* x + y *)
  | Refine of string * expr list * string option
  | Refinable of string (* refinable *)

and  
 
 *)
var_def = (string * string)

and sstmt =
    Decl of (var_def * Ast.expr option * environment)
  | If of ((Ast.expr option * sstmt list) list * environment)
  | While of ((Ast.expr * sstmt list) * environment)
  | Expr of Ast.expr * environment
  | Return of Ast.expr option * environment
  | Super of Ast.expr list * environment


and func_def = {
  returns : string option;
  host    : string option;
  name    : string;
  static  : bool;
  formals : var_def list;
  body    : sstmt list;
  section : class_section;  (* Makes things easier later *)
}

and class_section = Publics | Protects | Privates | Refines | Mains
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

type program = class_def list