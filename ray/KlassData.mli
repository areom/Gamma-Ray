open StringModules
open Util

val fold_classes : GlobalData.class_data -> ('a -> Ast.class_def -> 'a) -> 'a -> 'a
val map_classes : GlobalData.class_data -> ('a StringMap.t -> Ast.class_def -> 'a StringMap.t) -> 'a StringMap.t
val dfs_errors : GlobalData.class_data -> (string -> 'a -> 'b -> ('a * 'b)) -> 'a -> 'b -> 'b

val build_class_data : Ast.class_def list -> (GlobalData.class_data, GlobalData.class_data_error) either
val build_class_data_test : Ast.class_def list -> (GlobalData.class_data, GlobalData.class_data_error) either

val append_leaf : GlobalData.class_data -> Ast.class_def -> (GlobalData.class_data, GlobalData.class_data_error) either
val append_leaf_test : GlobalData.class_data -> Ast.class_def -> (GlobalData.class_data, GlobalData.class_data_error) either

val print_class_data : GlobalData.class_data -> unit
val errstr : GlobalData.class_data_error -> string
