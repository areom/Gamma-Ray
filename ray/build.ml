open Printf
open Util
open Classinfo

let source_file = open_in Sys.argv.(1)

let output_string str =
    let _ = print_string str in
    print_newline ()

let source_cast =
    let tokens = Inspector.from_channel source_file in
    let (_) = output_string "Loaded tokens" in
    let ast = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    let (_) = output_string "Formed AST" in
    let klass_data_test = KlassData.build_class_data ast in
    (** May the legions of hell descend on this monadism. *)
    let klass_data = match klass_data_test with
        | Left(x) -> x
        | Right(issue) -> Printf.fprintf stderr "%s\n" (errstr issue); exit(1)
    in
    let (_) = output_string "Tabulated class data" in
    let sast = BuildSast.ast_to_sast klass_data ast in
    let (_) = output_string "Transformed from AST to SAST" in
    let cast = GenCast.sast_to_cast klass_data sast in
    let (_) = output_string "Transformed from SAST to CAST" in
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
