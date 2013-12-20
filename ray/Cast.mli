open StringModules

(* The detail of an expression *)
type cexpr_detail =
    | This
    | Null
    | Id of string * Sast.varkind (* name, local/instance *)
    | NewObj of string * string * cexpr list (* ctype * fname * args *)
    | Literal of Ast.lit
    | Assign of cexpr * cexpr  (* memory := data -- whether memory is good is a semantic issue *)
    | Deref of cexpr * cexpr (* road[pavement] *)
    | Field of cexpr * string (* road.pavement *)
    | Invoc of cexpr * string * cexpr list (*Invoc(receiver, functionname, args) *)
    | Unop of Ast.op * cexpr (* !x *)
    | Binop of cexpr * Ast.op * cexpr (* x + y *)
    | Refine of cexpr list * string option * Sast.refine_switch (* arg list, opt ret type, switch list (class, uids) *)
    | Refinable of Sast.refine_switch (* list of classes supporting refinement *)

(* The expression and its type *)
and cexpr = string * cexpr_detail

(* A statement which has cexpr detail *)
and cstmt =
    | Decl of Ast.var_def * cexpr option * Sast.environment
    | If of (cexpr option * cstmt list) list * Sast.environment
    | While of cexpr * cstmt list * Sast.environment
    | Expr of cexpr * Sast.environment
    | Return of cexpr option * Sast.environment

(* A c func is a simplified function (no host, etc) *)
and cfunc = {
    returns : string option;
    name    : string;  (* Combine uid and name into this *)
    static  : bool;
    formals : Ast.var_def list;
    body    : cstmt list;
    builtin : bool;
    inklass : string; (* needed for THIS *)
}

(* The bare minimum for a struct represention *)
type class_struct = (string * Ast.var_def list) list (* All the data for this object from the root (first item) down, paired with class name *)

(* A main is a class name and a function name for that main *)
type main_func = (string * string)

(* We actually need all the ancestry information, cause we're gonna do it the right way [lists should go from object down] *)
type ancestry_info = (string list) lookup_map

(* A program is a map from all classes to their struct's, a list of all functions, and a list of mainfuncs, and ancestor information *)
type program = class_struct lookup_map * cfunc list * main_func list * ancestry_info
