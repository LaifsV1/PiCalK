(* BMC-2 Top Level *)
(*author: Yu-Yang Lin, date:2020*)
open Kast
open Format
open Lexing


let from_file file = Lexing.from_channel (open_in file)

let _ =
  let main_body () =
    try 
      let file = Sys.argv.(1) in
      let lexbuf = from_file file in
      let new_parser = Parser.kast_toplevel Lexer.read in
      let new_kast = new_parser lexbuf in
      (printf "%s" (string_of_kast new_kast));
      print_newline ()
    with
    | SyntaxError (msg,(p1,p2)) -> printf "    @[[SYNTAX ERROR]:@] @. ";
                                   printf "@[%s @] @." msg;
                                   printf "    @[%s @] @." (line_sprintf p1 p2);
                                   print_newline ();
                                   exit 1
    | Parser.Error -> printf "    @[[SYNTAX ERROR]: couldn't parse file.@] @. ";
                      print_newline ();
                      exit 1
  in
  main_body ()
