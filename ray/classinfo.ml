open Util
open Klass

let args lst = Format.sprintf "(%s)" (String.concat ", " lst)
let asig (name, formals) = Format.sprintf "%s %s" name (args formals)
let aref (name, formals) = asig (name, formals)

let dupvar (klass, vars) = match vars with
    | [var] -> "Class " ^ klass ^ "'s instance variable " ^ var ^ " is multiply declared"
    | _ -> "Class " ^ klass ^ " has multiply declared variables: [" ^ (String.concat ", " vars) ^ "]"

let dupmeth (klass, meths) =
    match meths with
        | [(name, formals)] -> Format.sprintf "Class %s's method %s has multiple implementations taking %s" klass name (args formals)
        | _ -> Format.sprintf "Class %s has multiple methods with conflicting signatures:\n\t%s" klass (String.concat "\n\t" (List.map asig meths))

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
    | ConflictingMethods(list) -> String.concat "\n" (List.map dupmeth list)
    | ConflictingRefinements(list) -> String.concat "\n" (List.map dupref list)
    | MultipleMains(klasses) -> (match klasses with
        | [klass] -> "Class " ^ klass ^ " has multiple mains defined."
        | _ -> "Multiple classes have more than one main: [" ^ String.concat ", " klasses ^ "]")

let _ =
    let tokens = Inspector.from_channel stdin in
    let classes = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    match Klass.build_class_data classes with
        | Left(data) -> Klass.print_class_data data; exit(0)
        | Right(issue) -> Printf.fprintf stderr "%s" (errstr issue); exit(1)

