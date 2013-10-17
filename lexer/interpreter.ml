open Ast

module NameMap = Map.Make(struct

type t = string

let compare x y = Pervasives.compare x y

end)

exception ReturnException of int * int NameMap.t

(* Main entry point: run a program *)

let run (vars, funcs) =

(* Put function declarations in a symbol table: map names to fdecls *)

let func_decls = List.fold_left

(fun funcs fdecl -> NameMap.add fdecl.fname fdecl funcs)

NameMap.empty funcs

in

(* Invoke a function and return an updated global symbol table *)

let rec call fdecl actuals globals =(* Evaluate an expression and return (value, updated environment) *)

let rec eval env = function

Literal(i) -> i, env

| Noexpr -> 1, env (* must be non-zero for the "for" loop predicate *)

| Id(var) ->

let locals, globals = env in

if NameMap.mem var locals then (* is it a local variable? *)

(NameMap.find var locals), env (* yes; return its value *)

else if NameMap.mem var globals then (* is it a global variable? *)

(NameMap.find var globals), env (* yes; return its value *)

else raise (Failure ("undeclared identifier " ^ var)) (* no: fail *)/* MicroC example */

int a; /* Global variable */

inca() { a = a + 1; return a; } /* Increment a; return its new value */

main() {

a = 0; /* Initialize a */

print(inca() + a); /* What should this print? */

}

| Binop(e1, op, e2) ->

let v1, env = eval env e1 in

let v2, env = eval env e2 in

let boolean i = if i then 1 else 0 in

(match op with

Add -> v1 + v2

| Sub -> v1 - v2

| Mult -> v1 * v2

| Div -> v1 / v2

| Equal -> boolean (v1 = v2)

| Neq -> boolean (v1 != v2)

| Less -> boolean (v1 < v2)

| Leq -> boolean (v1 <= v2)

| Greater -> boolean (v1 > v2)

| Geq -> boolean (v1 >= v2)), env| Assign(var, e) ->

let v, (locals, globals) = eval env e in (* Evaluate e *)

if NameMap.mem var locals then

v, (NameMap.add var v locals, globals) (* Update the local *)

else if NameMap.mem var globals then

v, (locals, NameMap.add var v globals) (* Update the global *)

else raise (Failure ("undeclared identifier " ^ var))

| Call("print", [e]) -> (* Built-in "print" is a special case *)

let v, env = eval env e in

print_endline (string_of_int v);

0, env/* Test argument evaluation order */

int a; /* Global variable */

inca() { a = a + 1; return a; } /* Increment a; return its new value */

add2(x, y) { return x + y; }

main() {

a = 0;

print(add2(inca(), a));

}

| Call(f, actuals) -> (* Call a user-defined function *)

let fdecl = (* Locate the caller’s declaration *)

try NameMap.find f func_decls

with Not_found -> raise (Failure ("undefined function " ^ f))

in

let actuals, env = List.fold_left (* Evaluate the actuals *)

(fun (actuals, env) actual ->

let v, env = eval env actual in (* capture any side-effect *)

v :: actuals, env)

([], env) (List.rev actuals)

in

let (locals, globals) = env in

try

let globals =

call fdecl actuals globals (* Pass actuals *)

in 0, (locals, globals) (* If it returns, treat value as 0 *)

with ReturnException(v, globals) ->

v, (locals, globals) (* Catch return value *)

in(* Execute a statement and return an updated environment:

(local symbol table, global symbol table) *)

let rec exec env = function

Block(stmts) ->

List.fold_left exec env stmts (* Remember side-effects *)

| Expr(e) ->

let _, env = eval env e in (* Evaluate expression *)

env (* Return any side-effects *)

| If(e, s1, s2) ->

let v, env = eval env e in (* Evaluate predicate *)

exec env (if v != 0 then s1 else s2) (* Run "then" or "else" *)

| While(e, s) ->

let rec loop env =

let v, env = eval env e in (* Evaluate predicate *)

if v != 0 then loop (exec env s) else env (* run body, repeat *)

in loop env| For(e1, e2, e3, s) ->

let _, env = eval env e1 in (* Loop initialization *)

let rec loop env =

let v, env = eval env e2 in (* Check predicate *)

if v != 0 then

let _, env = eval (exec env s) e3 (* Run body, update *)

in loop env (* and repeat *)

else

env

in loop env

| Return(e) ->

let v, (locals, globals) = eval env e in (* Evaluate value *)

raise (ReturnException(v, globals)) (* Pass result as exception *)

in(* Body of the call function: run the function *)

(* Bind actuals to formals:

foo(a, b) { ... } Declaration: a, b are formal arguments

foo(2 + 3, 42); Call site: 2+3, 42 are actual arguments

Bind a <= 5, b <= 42 *)

let locals =

try List.fold_left2

(fun locals formal actual -> NameMap.add formal actual locals)

NameMap.empty fdecl.formals actuals

with Invalid_argument(_) ->

raise (Failure ("wrong number of arguments to " ^ fdecl.fname))

in

(* Set locally declared variables to 0 *)

let locals = List.fold_left

(fun locals local -> NameMap.add local 0 locals)

locals fdecl.locals

in

(* Execute each statement; return updated global symbol table *)

snd (List.fold_left exec (locals, globals) fdecl.body)(* Body of the run function: run the program *)

(* set declared global variables to 0 *)

in let globals = List.fold_left

(fun globals vdecl -> NameMap.add vdecl 0 globals)

NameMap.empty vars

in

(* find and run the "main" function with no parameters *)

try

call (NameMap.find "main" func_decls) [] globals

with Not_found ->

raise (Failure ("did not find the main() function"))
