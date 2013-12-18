open Printf
open Util

let output_string whatever =
    print_string whatever;
    print_newline()

let load_file filename =
    if Sys.file_exists filename
        then open_in filename
        else raise Not_found;;

let with_file f file =
    let input = load_file file in
    let result = f input in
    close_in input;
    result

let get_data ast = match KlassData.build_class_data ast with
    | Left(data) -> data
    | Right(issue) -> Printf.fprintf stderr "%s" (KlassData.errstr issue); exit(1)

let source_cast =
    let tokens = with_file Inspector.from_channel Sys.argv.(1) in
    let ast = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    let klass_data = get_data ast in
    let sast = BuildSast.ast_to_sast klass_data ast in
    GenCast.sast_to_cast klass_data sast

let _ =
    try
        Printexc.record_backtrace true;
        GenC.cast_to_c source_cast stdout
    with _ ->
        Printexc.print_backtrace stderr
