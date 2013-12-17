open Printf

let source_file = open_in Sys.argv(1)

let source_cast =
    let tokens = Inspector.from_channel source_file in
    let ast = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    let klass_data = KlassData.build_klass_data ast in
    let sast = BuildSast.ast_to_sast klass_data ast in
    let cast = GenCast.sast_to_cast klass_data sast in
    cast

let output_h = open_out (Sys.argv(2) ^ ".h")
let output_c = open_out (Sys.argv(2) ^ ".c")

let print_header =
    let gen_header = GenC.cast_to_h cast in
    fprint output_h gen_header;
    close_out output_h

let print_source = GenC.cast_to_c cast
    let gen_source = GenC.cast_to_c cast in
    fprint output_c gen_source;
    close_out output_c
