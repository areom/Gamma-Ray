open Util

let _ =
    try
        Printexc.record_backtrace true;
        let tokens = Inspector.from_channel stdin in
        let classes = Parser.cdecls (WhiteSpace.lextoks tokens) (Lexing.from_string "") in
        let builder = if (Array.length Sys.argv = 1) then KlassData.build_class_data else KlassData.build_class_data_test in
        match builder classes with
            | Left(data) -> KlassData.print_class_data data; exit(0)
            | Right(issue) -> Printf.fprintf stderr "%s\n" (KlassData.errstr issue); exit(1)
    with _ ->
        Printexc.print_backtrace stderr
