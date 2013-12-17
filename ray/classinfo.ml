open Util
open Klass
open GlobalData

let args lst = Format.sprintf "(%s)" (String.concat ", " lst)
let asig (name, formals) = Format.sprintf "%s %s" name (args formals)
let aref (name, formals) = asig (name, formals)

let dupvar (klass, vars) = match vars with
    | [var] -> "Class " ^ klass ^ "'s instance variable " ^ var ^ " is multiply declared"
    | _ -> "Class " ^ klass ^ " has multiply declared variables: [" ^ (String.concat ", " vars) ^ "]"

let dupfield (klass, fields) = match fields with
    | [(ancestor, var)] -> "Class " ^ klass ^ "'s instance variable " ^ var ^ " was declared in ancestor " ^ ancestor ^ "."
    | _ -> "Class " ^ klass ^ " has instance variables declared in ancestors: [" ^ String.concat ", " (List.map (fun (a, v) -> v ^ " in " ^ a) fields) ^ "]"

let unknowntypes (klass, types) = match types with
    | [(vtype, vname)] -> "Class " ^ klass ^ "'s instancevariable " ^ vname ^ " has unknown type " ^ vtype ^ "."
    | _ -> "Class " ^ klass ^ " has instance variables with unknown types: [" ^ String.concat ", " (List.map (fun (t,v) -> t ^ ":" ^ v) types) ^ "]"

let dupmeth (klass, meths) =
    match meths with
        | [(name, formals)] -> Format.sprintf "Class %s's method %s has multiple implementations taking %s" klass name (args formals)
        | _ -> Format.sprintf "Class %s has multiple methods with conflicting signatures:\n\t%s" klass (String.concat "\n\t" (List.map asig meths))

let dupinherit (klass, meths) =
    match meths with
        | [(name, formals)] -> Format.sprintf "Class %s's method %s has conflicts with an inherited method taking %s" klass name (args formals)
        | _ -> Format.sprintf "Class %s has multiple methods with conflicting with inherited methods:\n\t%s" klass (String.concat "\n\t" (List.map asig meths))

let dupref (klass, refines) =
    match refines with
        | [refine] -> Format.sprintf "Class %s refinment %s is multiply defined." klass (aref refine)
        | _ -> Format.sprintf "Class %s has multiple refinements multiply defined:\n\t%s" klass (String.concat "\n\t" (List.map aref refines))

let errstr = function
    | HierarchyIssue(s) -> s
    | DuplicateClasses(klasses) -> (match klasses with
        | [klass] -> "Multiple classes named " ^ klass
        | _ -> "Multiple classes share the names [" ^ (String.concat ", " klasses) ^ "]")
    | DuplicateVariables(list) -> String.concat "\n" (List.map dupvar list)
    | DuplicateFields(list) -> String.concat "\n" (List.map dupfield list)
    | UnknownTypes(types) -> String.concat "\n" (List.map unknowntypes types)
    | ConflictingMethods(list) -> String.concat "\n" (List.map dupmeth list)
    | ConflictingInherited(list) -> String.concat "\n" (List.map dupinherit list)
    | Uninstantiable(klasses) -> (match klasses with
        | [klass] -> "Class " ^ klass ^ " does not have a usable init."
        | _ -> "Multiple classes are not instantiable: [" ^ String.concat ", " klasses ^ "]")
    | ConflictingRefinements(list) -> String.concat "\n" (List.map dupref list)
    | MultipleMains(klasses) -> (match klasses with
        | [klass] -> "Class " ^ klass ^ " has multiple mains defined."
        | _ -> "Multiple classes have more than one main: [" ^ String.concat ", " klasses ^ "]")

let _ =
    try
        Printexc.record_backtrace true;
        let tokens = Inspector.from_channel stdin in
        let classes = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
        let builder = if (Array.length Sys.argv = 1) then KlassData.build_class_data else KlassData.build_class_data_test in
        match builder classes with
            | Left(data) -> KlassData.print_class_data data; exit(0)
            | Right(issue) -> Printf.fprintf stderr "%s\n" (errstr issue); exit(1)
    with _ ->
        Printexc.print_backtrace stderr
