open Printf
open Util

let output_string whatever =
    print_string whatever;
    print_newline()

let load_file filename =
    if Sys.file_exists filename
        then open_in filename
        else raise Not_found;;

let source_file = load_file Sys.argv.(1)

let source_cast =
    let tokens = Inspector.from_channel source_file in
    close_in source_file;
    output_string "Loaded tokens";
    let ast = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    output_string "Formed AST";
    let klass_data_test = KlassData.build_class_data ast in
    (** May the legions of hell descend on this monadism. *)
    let klass_data = match klass_data_test with
        | Left(x) -> x
        | Right(issue) -> Printf.fprintf stderr "%s\n" ("Really need to move the error function out of classinfo"); exit(1)
    in
    output_string "Tabulated class data";
    let sast = BuildSast.ast_to_sast klass_data ast in
    output_string "Transformed from AST to SAST";
    let cast = GenCast.sast_to_cast klass_data sast in
    output_string "Transformed from SAST to CAST";
    cast

let output_h = open_out (Sys.argv.(2) ^ ".h")
let output_c = open_out (Sys.argv.(2) ^ ".c")

let print_header =
    let gen_header = GenC.cast_to_h source_cast in
    fprintf output_h "%s" gen_header;
    close_out output_h

let print_source =
    let gen_source = GenC.cast_to_c source_cast in
    fprintf output_c "%s" gen_source;
    close_out output_c 
