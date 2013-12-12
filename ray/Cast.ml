type varkind = Instance | Local

type environment = (string * varkind) Map.Make(String).t

type cexpr_detail =
    | This
    | Null
    | Id of string
(*    | NewObj of string * expr list
    | Anonymous of string * Ast.expr list * Ast.func_def list (* Evaluation is delayed *)
*)
    | Literal of Ast.lit
    | Assign of cexpr * cexpr  (* memory := data -- whether memory is good is a semantic issue *)
    | Deref of cexpr * cexpr (* road[pavement] *)
(*
    | Field of expr * string (* road.pavement *)
    | Invoc of expr * string * expr list (* receiver.method(args) *)
*) 
   | Unop of Ast.op * cexpr (* !x *)
    | Binop of cexpr * Ast.op * cexpr (* x + y *)
(* 
   | Refine of string * expr list * string option
    | Refinable of string (* refinable *)
*)
and  

cexpr = string * cexpr_detail

and 
  
cvar_def = (string * string)

and cstmt =
    | Decl of cvar_def * cexpr option * environment
    | If of (cexpr option * cstmt list) list * environment
    | While of cexpr * cstmt list * environment
    | Expr of cexpr * environment
    | Return of cexpr option * environment
(*    | Super of expr list * environment*)
(*

and cfunc_def = {
    returns : string option;
    host    : string option;
    name    : string;
    static  : bool;
    formals : cvar_def list;
    body    : sstmt list;
    section : Ast.class_section;  (* Makes things easier later *)
    inklass : string;
}

(* A member is either a variable or some sort of function *)
type member_def = VarMem of cvar_def | MethodMem of func_def | InitMem of func_def

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
*)
