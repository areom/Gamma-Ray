(*type varkind = Instance | Local

type environment = (string * varkind) Map.Make(String).t
*)
type cexpr_detail =
    | This
    | Null
    | Id of string
    | NewObj of string * string * cexpr list (* ctype * fname * args *)
(*  | Anonymous of string * Ast.expr list * Ast.func_def list (* Evaluation is delayed *)
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


and cfunc = {
    returns : string option;
    uid     : string;
    static  : bool;
    formals : cvar_def list;
    body    : cstmt list;
    builtin : bool;
}
	
(* The bare minimum for a struct represention *)
type class_struct = {
    klass     : string list; (* It's important to know all the possible classes, in ascending order up to Object. The first one becomes the name. *)
    refines   : string list; (* The uids of all the refinements *)
    variables : cvar_def list;
}

type program_entry = CFunction of class_struct | ClassStruct of cfunc | MainInvoke of string

type program = program_entry list
