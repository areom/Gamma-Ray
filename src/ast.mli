type lit =
  Int of int
  | Float of float
  | String of string
  | Bool of bool

type arith = Add | Sub | Prod | Quot | Div | Mod | Neg | Pow

type numtest = Eq | Neq | Less | Grtr | Leq | Geq

type combtest = And | Or | Nand | Nor | Xor | Not

type op = Arithmatic of arith | NumTest of numtest | CombTest of combtest

type expr =
    Id of string
  | Literal of lit
  | Invoc of expr * expr * expr list (* object.function (args), args and object can be Noexpr*)
  | Member of expr * expr (* road.pavement *)
  | Deref of expr * expr (* road[pavement] *)
  | Unop of op * expr (* !x *)
  | Binop of expr * op * expr (* x + y *)
  | Noexpr

type var_def = string * string (* Oh typing, you pain in the ass *)

type stmt =
    Block of stmt list
  | Decl of var_def
  | Assign of string * expr (* as our grammer is written, assignment is a statement. Do we want this? *)
  | If of expr * stmt list * stmt list (* Unless we program in optimization, this is bothersomely inefficient for elsif *)
  | While of expr * stmt list
  | Expr of expr
  | Return of expr

(* both functions and refinements *)
type func_def = {
  fname : string;
  fstatic : bool;
  formals : var_def list;
  locals : var_def list;
  body : stmt list;
}

(* Just pop init and main in there? *)
type class_def = {
  cname : string;
  csource : class_def;
  cprivate : func_def list * decl list;
  cprotected : func_def list * decl list;
  cpublic : func_def list * decl list;
  crefine : func_def list;
}
