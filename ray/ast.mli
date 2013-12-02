(**
    The abstract syntax tree for Gamma
*)

(**
    The four literal classes of Gamma:
    - Int - Integer
    - Float - Floating-point number
    - String - A sequence of characters
    - Bool - a boolean value of either true or false
*)
type lit =
    Int of int
  | Float of float
  | String of string
  | Bool of bool

(** The binary arithmatic operators *)
type arith = Add | Sub | Prod | Quot | Div | Mod | Neg | Pow

(** The binary comparison operators *)
type numtest = Eq | Neq | Less | Grtr | Leq | Geq

(** The binary boolean operators *)
type combtest = And | Or | Nand | Nor | Xor | Not

(** All three sets of binary operators *)
type op = Arithmetic of arith | NumTest of numtest | CombTest of combtest

(**
    The set expressions:
        This
*)
type expr =
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
(** The basic variable definition, a type and an id*)
and var_def = string * string  (* Oh typing, you pain in the ass, add a int for array *)
(** The basic statements: Variable declarations, control statements, assignments, return statements, and super class expressions *)
and stmt =
    Decl of var_def * expr option
  | If of (expr option * stmt list) list
  | While of expr * stmt list
  | Expr of expr
  | Return of expr option
  | Super of expr list

(** We have four different kinds of callable code blocks:
    - main: only has formals, body (name / static / host known)
    - init: only has formals, body (name / static / host known)
    - refine: has host, name, formals, body (static known)
    - method: has name, formals, body (host / static known)
*)

(** Three access levels, the refinements, and the main function *)
and class_section = Publics | Protects | Privates | Refines | Mains

(** The basic function definition *)
and func_def = {
  returns : string option;  (** A return type *)
  host    : string option;  (** A host class *)
  name    : string;         (** The function name *)
  static  : bool;           (** If the function is static *)
  formals : var_def list;   (** A list of all formal parameters of the function *)
  body    : stmt list;      (** A list of statements that form the function body *)
  section : class_section;  (** A semenantic tag of the class section in which the function lives *)
}

(** A member is either a variable or some sort of function *)
type member_def = VarMem of var_def | MethodMem of func_def | InitMem of func_def

(** Things that can go in a class *)
type class_sections_def = {
  privates : member_def list;
  protects : member_def list;
  publics  : member_def list;
  refines  : func_def list;
  mains    : func_def list;
}
	
(* Just pop init and main in there? *)
(** The basic class definition *)
type class_def = {
  klass    : string; (** A name string *)
  parent   : string option; (** The parent class name *)
  sections : class_sections_def; (** The five sections *)
}

(** A program, right and proper  *)
type program = class_def list
