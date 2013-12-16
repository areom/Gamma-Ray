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

let mem f = if f.name = "init" then InitMem(f) else MethodMem(f)
let members = List.map mem

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

    let sections : Ast.class_sections_def =
        { sections with
          publics = [mem get_id];
          protects = [mem init_obj] } in

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
          name = "printInt";
          formals = [("Int", "arg")];
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
          publics = members [print_string; print_int; print_float; print_init] } in

    { klass = name; parent = Some("Object"); sections = sections }

let class_string : Ast.class_def =
    let name = "String" in
    let built_in = { built_in with inklass = name } in

    let string_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "string_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [mem string_init] } in

    { klass = name; parent = Some("Object"); sections = sections }


let class_boolean : Ast.class_def =
    let name = "Boolean" in
    let built_in = { built_in with inklass = name } in

    let boolean_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "boolean_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [mem boolean_init] } in

    { klass = name; parent = Some("Object"); sections = sections }

let class_int : Ast.class_def =
    let name = "Int" in
    let built_in = { built_in with inklass = name } in

    let int_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "int_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [mem int_init] } in

    { klass = name; parent = Some("Object"); sections = sections }

let class_float : Ast.class_def =
    let name = "Float" in
    let built_in = { built_in with inklass = name } in

    let float_init : Ast.func_def =
        { built_in with
          name = "init";
          uid = "float_init" } in

    let sections : Ast.class_sections_def =
        { sections with
          protects = [mem float_init] } in

    { klass = name; parent = Some("Object"); sections = sections }

(** The list of built in classes and their methods *)
let built_in_classes =
  [ class_object; class_string; class_boolean; class_int; class_float ]

