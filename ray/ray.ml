open Printf
open Util

let output_string whatever =
    print_string whatever;
    print_newline()

let load_file filename =
    if Sys.file_exists filename
        then open_in filename
        else raise(Failure("Could not find file " ^ filename ^ "."))

let with_file f file =
    let input = load_file file in
    let result = f input in
    close_in input;
    result

let get_data ast =
    let (which, builder) = if (Array.length Sys.argv <= 2)
        then ("Normal", KlassData.build_class_data)
        else ("Experimental", KlassData.build_class_data_test) in
    output_string (Format.sprintf "Using %s KlassData Builder" which);
    match builder ast with
        | Left(data) -> data
        | Right(issue) -> Printf.fprintf stderr "%s" (KlassData.errstr issue); exit(1)

let source_cast =
    output_string "Reading Tokens...";
    let tokens = with_file Inspector.from_channel Sys.argv.(1) in
    output_string "Parsing Tokens...";
    let ast = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    output_string "Generating Global Data...";
    let klass_data = get_data ast in
    output_string "Building Semantic AST...";
    let sast = BuildSast.ast_to_sast klass_data ast in
    output_string "Generating C AST...";
    GenCast.sast_to_cast klass_data sast

let main _ =
    Printexc.record_backtrace true;
    output_string "Generating C...";
    try
        GenC.cast_to_c source_cast stdout;
        print_newline ()
    with _ ->
        output_string "Got an exception!";
        Printexc.print_backtrace stderr

let _ = main ()

