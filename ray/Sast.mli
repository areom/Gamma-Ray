
(** Types for the semantic abstract syntax tree *)

(** A switch for refinment or refinable checks *)
type refine_switch =
    | Switch of string * (string * string) list * string (* host class,  class/best-uid list, switch uid *)
    | Test of string * string list * string  (* host class, class list, uid of switch *)

(** The type of a variable in the environment *)
type varkind = Instance of string | Local

(** The environment at any given statement. *)
type environment = (string * varkind) Map.Make(String).t

(** The ID can be built in (and so won't get mangled) or an array allocator. *)
type funcid = BuiltIn of string | FuncId of string | ArrayAlloc of string

(** An expression value -- like in AST *)
type expr_detail =
    | This
    | Null
    | Id of string
    | NewObj of string * expr list * funcid
    | NewArr of string * expr list * funcid
    | Anonymous of string * Ast.expr list * Ast.func_def list (* Evaluation is delayed *)
    | Literal of Ast.lit
    | Assign of expr * expr  (* memory := data -- whether memory is good is a semantic issue *)
    | Deref of expr * expr (* road[pavement] *)
    | Field of expr * string (* road.pavement *)
    | Invoc of expr * string * expr list * funcid (* receiver.method(args) * bestmethod_uid  *)
    | Unop of Ast.op * expr (* !x *)
    | Binop of expr * Ast.op * expr (* x + y *)
    | Refine of string * expr list * string option * refine_switch (* refinement, arg list, opt ret type, switch *)
    | Refinable of string * refine_switch (* desired refinement, list of classes supporting refinement *)

(** An expression with a type tag *)
and expr = string * expr_detail

(** A statement tagged with an environment *)
and sstmt =
    | Decl of Ast.var_def * expr option * environment
    | If of (expr option * sstmt list) list * environment
    | While of expr * sstmt list * environment
    | Expr of expr * environment
    | Return of expr option * environment
    | Super of expr list *string * environment (**arglist, uidof super init, env**)

(** A function definition *)
and func_def = {
    returns : string option;
    host    : string option;
    name    : string;
    static  : bool;
    formals : Ast.var_def list;
    body    : sstmt list;
    section : Ast.class_section;  (* Makes things easier later *)
    inklass : string;
    uid     : string;
    builtin : bool;
}

(* A member is either a variable or some sort of function *)
type member_def = VarMem of Ast.var_def | MethodMem of func_def | InitMem of func_def

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
