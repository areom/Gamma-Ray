open Ast

(** Built in classes *)

let built_in : Ast.func_def =
    { returns = None;
      host = None;
      name = "";
      static = false;
      formals = [];
      body = [];
      section = Publics;
      inklass = "";
      uid = "";
      builtin = true }

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
    let built_in = { built_in with inklass = name } in

    let get_id : Ast.func_def =
        { built_in with
          returns = Some("String");
          name = "getId";
          uid = "object_get_id" } in
    let init_obj : Ast.func_def =
        { built_in with
          name = "init";
          section = Protects;
          uid = "object_init" } in
    let system = ("System", "system") in

    let sections : Ast.class_sections_def =
        { sections with
          publics = [func get_id];
          protects = [func init_obj; var system] } in

    { klass = name; parent = None; sections = sections }

let class_scanner : Ast.class_def =
    let name = "Scanner" in
    let built_in = { built_in with inklass = name } in

    let scan_line : Ast.func_def =
        { built_in with
          name = "scanString";
          returns = Some("String");
          uid = "scanner_scan_string" } in
    let scan_int : Ast.func_def =
        { built_in with
          name = "scanInteger";
          returns = Some("Integer");
          uid = "scanner_scan_integer" } in
    let scan_float : Ast.func_def =
        { built_in with
          name = "scanFloat";
          returns = Some("Float");
          uid = "scanner_scan_float" } in
    let scan_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "scanner_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          publics = functions [scan_line; scan_int; scan_float; scan_init] } in

    { klass = name; parent = None; sections = sections }

let class_printer : Ast.class_def =
    let name = "Printer" in
    let built_in = { built_in with inklass = name } in

    let print_string : Ast.func_def =
        { built_in with
          name = "printString";
          formals = [("String", "arg")];
          uid = "printer_print_string" } in
    let print_int : Ast.func_def =
        { built_in with
          name = "printInteger";
          formals = [("Integer", "arg")];
          uid = "printer_print_int" } in
    let print_float : Ast.func_def =
        { built_in with
          name = "printFloat";
          formals = [("Float", "arg")];
          uid = "printer_print_float" } in
    let print_init : Ast.func_def =
        { built_in with
          name = "init";
          formals = [("Boolean", "stdout")];
          uid = "printer_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          publics = functions [print_string; print_int; print_float; print_init] } in

    { klass = name; parent = None; sections = sections }

let class_string : Ast.class_def =
    let name = "String" in
    let built_in = { built_in with inklass = name } in

    let string_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "string_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [func string_init] } in

    { klass = name; parent = None; sections = sections }


let class_boolean : Ast.class_def =
    let name = "Boolean" in
    let built_in = { built_in with inklass = name } in

    let boolean_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "boolean_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [func boolean_init] } in

    { klass = name; parent = None; sections = sections }

let class_integer : Ast.class_def =
    let name = "Integer" in
    let built_in = { built_in with inklass = name } in

    let integer_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "integer_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [func integer_init] } in

    { klass = name; parent = None; sections = sections }

let class_float : Ast.class_def =
    let name = "Float" in
    let built_in = { built_in with inklass = name } in

    let float_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "float_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [func float_init] } in

    { klass = name; parent = None; sections = sections }

let class_system : Ast.class_def =
    let name = "System" in
    let built_in = { built_in with inklass = name } in

    let system_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "system_init" } in
    let system_exit : Ast.func_def =
        { built_in with
          name = "exit";
          formals = [("Integer", "code")];
          uid = "system_exit"; } in
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
