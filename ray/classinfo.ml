open Util

let show_classes builder classes = match builder classes with
    | Left(data) -> KlassData.print_class_data data; exit(0)
    | Right(issue) -> Printf.fprintf stderr "%s\n" (KlassData.errstr issue); exit(1)

let from_input builder =
    let tokens = Inspector.from_channel stdin in
    let classes = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
    show_classes builder classes
let from_basic builder = show_classes builder []

let basic_info_test () = from_basic KlassData.build_class_data_test
let basic_info () = from_basic KlassData.build_class_data

let test_info () = from_input KlassData.build_class_data_test
let normal_info () = from_input KlassData.build_class_data

let exec name func = Printf.printf "Executing mode %s\n" name; flush stdout; func ()

let _ = try
    Printexc.record_backtrace true;
    match Array.to_list Sys.argv with
        | []     -> raise(Failure("Not even program name given as argument."))
        | [_]    -> exec "Normal Info" normal_info
        | _::arg::_ -> match arg with
        | "-"    -> exec "Basic Info"  basic_info
        | "--"   -> exec "Basic Test"  basic_info_test
        | _      -> exec "Test Info"   test_info
with _ ->
    Printexc.print_backtrace stderr
