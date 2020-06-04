(* lts_builder toplevel *)
(*author: Yu-Yang Lin, date: June-2020*)
open Kast
open Kast_tools
open Format
open Lexing

let from_file file = Lexing.from_channel (open_in file)

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let _ =
  let main_body () =
    try 
      let file = Sys.argv.(1) in
      let new_kast = kast_of_string (read_whole_file file) in
      printf "%s" (string_of_kast (krun_kast new_kast))
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
  
