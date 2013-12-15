val token_to_string : Parser.token -> string
val descan : Parser.token -> string
val token_list : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parser.token list
val from_channel : Pervasives.in_channel -> Parser.token list
val pprint_token_list : string -> Parser.token list -> unit
val pprint_token_lines : string -> (int * Parser.token list * bool) list -> unit
val inspect_ast_lit : Ast.lit -> string
val inspect_ast_arith : Ast.arith -> string
val inspect_ast_numtest : Ast.numtest -> string
val inspect_ast_combtest : Ast.combtest -> string
val inspect_ast_op : Ast.op -> string
val inspect_ast_expr : Ast.expr -> string
val inspect_ast_var_def : Ast.var_def -> string
val inspect_ast_stmt : Ast.stmt -> string
val inspect_ast_clause : Ast.expr option * Ast.stmt list -> string
val inspect_ast_class_section : Ast.class_section -> string
val inspect_ast_func_def : Ast.func_def -> string
val inspect_ast_member_def : Ast.member_def -> string
val inspect_ast_class_sections : Ast.class_sections_def -> string
val inspect_ast_class_def : Ast.class_def -> string
