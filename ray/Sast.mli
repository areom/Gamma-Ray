
type varkind = Instance | Local

(** Types for the semantic abstract syntax tree *)


type environment = (string * varkind) Map.Make(String).t

(*
type lit =
        Int of int
    | Float of float
    | String of string
    | Bool of bool

type arith = Add | Sub | Prod | Quot | Div | Mod | Neg | Pow
type numtest = Eq | Neq | Less | Grtr | Leq | Geq
type combtest = And | Or | Nand | Nor | Xor | Not
type op = Arithmetic of arith | NumTest of numtest | CombTest of combtest
*)
type expr_detail =
    | This
    | Null
    | Id of string
    | NewObj of string * expr list * string
    | Anonymous of string * Ast.expr list * Ast.func_def list (* Evaluation is delayed *)
    | Literal of Ast.lit
    | Assign of expr * expr  (* memory := data -- whether memory is good is a semantic issue *)
    | Deref of expr * expr (* road[pavement] *)
    | Field of expr * string (* road.pavement *)
    | Invoc of expr * string * expr list * string (* receiver.method(args) * bestmethod_uid  *)
    | Unop of Ast.op * expr (* !x *)
    | Binop of expr * Ast.op * expr (* x + y *)
    | Refine of string * expr list * string option * (string * string) list (* refinement, arg list, opt ret type, switch list (class, uids) *)
    | Refinable of string * string list (* desired refinement, list of classes supporting refinement *)

and

expr = string * expr_detail

and

var_def = (string * string)

and sstmt =
    | Decl of var_def * expr option * environment
    | If of (expr option * sstmt list) list * environment
    | While of expr * sstmt list * environment
    | Expr of expr * environment
    | Return of expr option * environment
    | Super of expr list *string * environment (**arglist, uidof super init, env**)


and func_def = {
    returns : string option;
    host    : string option;
    name    : string;
    static  : bool;
    formals : var_def list;
    body    : sstmt list;
    section : Ast.class_section;  (* Makes things easier later *)
    inklass : string;
    uid     : string;
    builtin : bool;
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

type program = class_def list
