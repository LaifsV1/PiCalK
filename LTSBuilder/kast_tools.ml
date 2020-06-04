(* KAST Tools *)
(*author: Yu-Yang Lin, date: June-2020*)
open Kast
open Krun
open Format
open Lexing

(* kast to string via tree traversal *)
let rec string_of_kast kast =
  match kast with
  | C(s , ks) -> sprintf "%s(%s)" s (string_of_kast_list ks)
  | H(s , ks) -> sprintf "%s(%s)" s (string_of_kast_list ks)
  | T(s , ks) -> sprintf "%s(%s)" s (string_of_kast_list ks)
  | S(s)      -> s
  | K(s)      -> s
  | D(s)      -> s
and string_of_kast_list ks =
  match ks with
  | [] -> ""
  | k::[] -> string_of_kast k
  | k::ks -> sprintf "%s,%s" (string_of_kast k) (string_of_kast_list ks)

(* string to kast via lexings and parsing *)
let from_string str = Lexing.from_string str
let kast_of_string str =
  let lexbuf = from_string str in
  let new_parser = Parser.kast_toplevel Lexer.read in
  let new_kast = new_parser lexbuf in
  new_kast

(* find all cells in a kast and list them *)
let rec list_T_cells kast acc =
  match kast with
  | C("`<T>`" , ks) -> kast::acc
  | C(_ , ks) -> list_T_cells_helper ks acc
  | H(_ , ks) -> list_T_cells_helper ks acc
  | T(_ , ks) -> list_T_cells_helper ks acc
  | S(_)      -> acc
  | K(_)      -> acc
  | D(_)      -> acc
and list_T_cells_helper ks acc =
  match ks with
  | [] -> acc
  | k::ks -> list_T_cells_helper ks (list_T_cells k acc)

let rec print_kast_list ks =
  match ks with
  | [] -> print_newline ()
  | k::ks -> printf "RESULT:\n%s\n" (string_of_kast k);
             print_kast_list ks

(* updated step cell in a T configuration *)
let rec update_step_cell_data i ks =
  match ks with
  | [] -> []
  | k::ks ->
     (match k with
      | C("`<step>`" , _) ->
         (C("`<step>`",
            [H("#token",[S(sprintf "\"%d\"" i);S("\"Int\"")])]))
         ::(update_step_cell_data i ks)
      | _ -> k::(update_step_cell_data i ks))
let rec apply_cell_data f kast =
  match kast with
  | C(x,ks) -> C(x,f ks)
  | _ -> failwith "error: get_T_cell_data"
let update_step_cell i kast =
  apply_cell_data (update_step_cell_data i) kast

                  (* krun from kast to kast *)
let krun_kast kast = kast_of_string (krun (string_of_kast kast))
