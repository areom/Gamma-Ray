
val ast_to_sast_klass : GlobalData.class_data -> Ast.class_def -> Sast.class_def
val ast_to_sast : GlobalData.class_data -> Sast.class_def list
val update_refinements : GlobalData.class_data -> Sast.class_def list -> Sast.class_def list
