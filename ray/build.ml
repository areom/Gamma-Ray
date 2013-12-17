open Printf
open Util
open Classinfo

let source_file = open_in Sys.argv.(1)

let source_cast =
    let tokens = Inspector.from_channel source_file in
    let ast = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    let klass_data_test = KlassData.build_class_data ast in
    (** May the legions of hell descend on this monadism. *)
    let klass_data = match klass_data_test with
        | Left(x) -> x
        | Right(issue) -> Printf.fprintf stderr "%s\n" (errstr issue); exit(1)
    in
    let sast = BuildSast.ast_to_sast klass_data ast in
    let cast = GenCast.sast_to_cast klass_data sast in
    cast

let output_h = open_out (Sys.argv.(2) ^ ".h")
let output_c = open_out (Sys.argv.(2) ^ ".c")

let print_header =
    let gen_header = GenC.cast_to_h cast in
    fprint output_h gen_header;
    close_out output_h

let print_source =
    let gen_source = GenC.cast_to_c cast in
    fprint output_c gen_source;
    close_out output_c
