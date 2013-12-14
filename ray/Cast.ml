(*type varkind = Instance | Local

type environment = (string * varkind) Map.Make(String).t
*)
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

    | Field of cexpr * string (* road.pavement *)
    | Invoc of cexpr * string * cexpr list (*Invoc(receiver, functionname, args) *)
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
    | Decl of cvar_def * cexpr option * Sast.environment
    | If of (cexpr option * cstmt list) list * Sast.environment
    | While of cexpr * cstmt list * Sast.environment
    | Expr of cexpr * Sast.environment
    | Return of cexpr option * Sast.environment
    | Super of cexpr list * Sast.environment


and cfunc_def = {
    returns : string option;
    uid     : string;
    static  : bool;
    formals : cvar_def list;
    body    : cstmt list;
}

(*
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
