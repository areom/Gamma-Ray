open Ast
open Str

(** Built in classes *)

let built_in cname : Ast.func_def = match Str.split (regexp "_") cname with
    | [] -> raise(Failure "Bad cname -- empty.")
    | [klass] -> raise(Failure("Bad cname -- just class: " ^ klass))
    | klass::func ->
        let methname = match func with
            | [] -> raise(Failure("Impossible!"))
            | func::rest -> func ^ (String.concat "" (List.map String.capitalize rest)) in
        { returns = None;
          host = None;
          name = methname;
          static = false;
          formals = [];
          body = [];
          section = Publics;
          inklass = String.capitalize klass;
          uid = cname;
          builtin = true }
let breturns cname atype = { (built_in cname) with returns = Some(atype) }
let btakes cname formals = { (built_in cname) with formals = formals }

let sections : Ast.class_sections_def =
    { publics = [];
      protects = [];
      privates = [];
      refines = [];
      mains = [] }

let func f = if f.name = "init" then InitMem(f) else MethodMem(f)
let var v = VarMem(v)
let variables = List.map var
let functions = List.map func
let members f v = (functions f) @ (variables v)

let class_object : Ast.class_def =
    let name = "Object" in

    let get_id : Ast.func_def = breturns "object_get_id" "String" in
    let id = ("Integer", "obj_id") in
    let init_obj : Ast.func_def = { (built_in "object_init") with section = Protects } in
    let system = ("System", "system") in

    let sections : Ast.class_sections_def =
        { sections with
          publics = [func get_id];
          protects = [func init_obj; var system];
          privates = [var id] } in

    { klass = name; parent = None; sections = sections }

let class_scanner : Ast.class_def =
    let name = "Scanner" in

    let scan_line : Ast.func_def = breturns "scanner_scan_string" "String" in
    let scan_int : Ast.func_def = breturns "scanner_scan_integer" "Integer" in
    let scan_float : Ast.func_def = breturns "scanner_scan_float" "Float" in
    let scan_init : Ast.func_def = built_in "scanner_init" in

    let sections : Ast.class_sections_def =
        { sections with
          publics = functions [scan_line; scan_int; scan_float; scan_init] } in

    { klass = name; parent = None; sections = sections }

let class_printer : Ast.class_def =
    let name = "Printer" in

    let print_string : Ast.func_def = btakes "printer_print_string" [("String", "arg")] in
    let print_int : Ast.func_def = btakes "printer_print_integer" [("Integer", "arg")] in
    let print_float : Ast.func_def = btakes "printer_print_float" [("Float", "arg")] in
    let print_init : Ast.func_def = btakes "printer_init" [("Boolean", "stdout")] in

    let sections : Ast.class_sections_def =
        { sections with
          publics = functions [print_string; print_int; print_float; print_init] } in

    { klass = name; parent = None; sections = sections }

let class_string : Ast.class_def =
    let name = "String" in

    let string_init : Ast.func_def = built_in "string_init" in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [func string_init] } in

    { klass = name; parent = None; sections = sections }


let class_boolean : Ast.class_def =
    let name = "Boolean" in

    let boolean_init : Ast.func_def = built_in "boolean_init" in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [func boolean_init] } in

    { klass = name; parent = None; sections = sections }

let class_integer : Ast.class_def =
    let name = "Integer" in

    let integer_init : Ast.func_def = built_in "integer_init" in
    let integer_float : Ast.func_def = breturns "integer_to_f" "Float" in

    let sections : Ast.class_sections_def =
        { sections with
          publics = [func integer_float];
          protects = [func integer_init] } in

    { klass = name; parent = None; sections = sections }

let class_float : Ast.class_def =
    let name = "Float" in

    let float_init : Ast.func_def = built_in "float_init" in
    let float_integer : Ast.func_def = breturns "float_to_i" "Integer" in

    let sections : Ast.class_sections_def =
        { sections with
          publics = [func float_integer];
          protects = [func float_init] } in

    { klass = name; parent = None; sections = sections }

let class_system : Ast.class_def =
    let name = "System" in

    let system_init : Ast.func_def = built_in "system_init" in
    let system_exit : Ast.func_def = btakes "system_exit" [("Integer", "code")] in

    let system_out = ("Printer", "out") in
    let system_err = ("Printer", "err") in
    let system_in = ("Scanner", "in") in

    let sections : Ast.class_sections_def =
        { sections with
          publics = members [system_init; system_exit] [system_out; system_err; system_in]; } in

    { klass = name; parent = None; sections = sections }

(** The list of built in classes and their methods *)
let built_in_classes =
  [ class_object; class_string; class_boolean; class_integer; class_float; class_printer; class_scanner; class_system ]
