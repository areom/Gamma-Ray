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
    output_string (Format.sprintf " * Using %s KlassData Builder" which);
    match builder ast with
        | Left(data) -> data
        | Right(issue) -> Printf.fprintf stderr "%s\n" (KlassData.errstr issue); exit 1

let do_deanon klass_data sast = match Unanonymous.deanonymize klass_data sast with
    | Left(result) -> result
    | Right(issue) -> Printf.fprintf stderr "Error Deanonymizing:\n%s\n" (KlassData.errstr issue); exit 1

let source_cast _ =
    output_string " * Reading Tokens...";
    let tokens = with_file Inspector.from_channel Sys.argv.(1) in
    output_string " * Parsing Tokens...";
    let ast = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    output_string " * Generating Global Data...";
    let klass_data = get_data ast in
    output_string " * Building Semantic AST...";
    let sast = BuildSast.ast_to_sast klass_data in
    output_string " * Deanonymizing Anonymous Classes.";
    let (klass_data, sast) = do_deanon klass_data sast in
    output_string " * Rebinding refinements.";
    let sast = BuildSast.update_refinements klass_data sast in
    output_string " * Generating C AST...";
    GenCast.sast_to_cast klass_data sast

let main _ =
    Printexc.record_backtrace true;
    output_string "/* Starting Build Process...";
    try
        let source = source_cast () in
        output_string " * Generating C...";
        output_string " */";
        GenC.cast_to_c source stdout;
        print_newline ();
        exit 0
    with excn ->
        let backtrace = Printexc.get_backtrace () in
        let reraise = ref false in
        let out = match excn with
            | Failure(reason) -> Format.sprintf "Failed: %s\n" reason
            | Invalid_argument(msg) -> Format.sprintf "Argument issue somewhere: %s\n" msg
            | Parsing.Parse_error -> "Parsing error."
            | _ -> reraise := true; "Unknown Exception" in
        Printf.fprintf stderr "%s\n%s\n" out backtrace;
        if !reraise then raise(excn) else exit 1

let _ = main ()
