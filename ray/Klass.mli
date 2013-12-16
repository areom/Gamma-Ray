open Util

val klass_to_parent : Ast.class_def -> string
val section_string : Ast.class_section -> string
val klass_to_variables : Ast.class_def -> (Ast.class_section * Ast.var_def list) list
val klass_to_methods : Ast.class_def -> (Ast.class_section * Ast.func_def list) list
val klass_to_functions : Ast.class_def -> (Ast.class_section * Ast.func_def list) list
val conflicting_signatures : Ast.func_def -> Ast.func_def -> bool
val signature_string : Ast.func_def -> string
val full_signature_string : Ast.func_def -> string
val class_var_lookup : GlobalData.class_data -> string -> string -> (Ast.class_section * string) option
val class_field_lookup : GlobalData.class_data -> string -> string -> (string * string * Ast.class_section) option
val class_field_far_lookup : GlobalData.class_data -> string -> string -> bool -> ((string * string * Ast.class_section), bool) either
val class_method_lookup : GlobalData.class_data -> string -> string -> Ast.func_def list
val class_ancestor_method_lookup : GlobalData.class_data -> string -> string -> bool -> Ast.func_def list
val refine_lookup : GlobalData.class_data -> string -> string -> string -> Ast.func_def list
val get_distance : GlobalData.class_data -> string -> string -> int option
val is_type : GlobalData.class_data -> string -> bool
val is_subtype : GlobalData.class_data -> string -> string -> bool
val is_proper_subtype : GlobalData.class_data -> string -> string -> bool
val compatible_formals : GlobalData.class_data -> string list -> string list -> bool
val compatible_function : GlobalData.class_data -> string list -> Ast.func_def -> bool
val compatible_return : GlobalData.class_data -> string option -> Ast.func_def -> bool
val best_matching_signature : GlobalData.class_data -> string list -> Ast.func_def list -> Ast.func_def list
val best_method : GlobalData.class_data -> string -> string -> string list -> Ast.class_section list -> Ast.func_def option
val best_inherited_method : GlobalData.class_data -> string -> string -> string list -> bool -> Ast.func_def option
val refine_on : GlobalData.class_data -> string -> string -> string -> string list -> string option -> Ast.func_def list
