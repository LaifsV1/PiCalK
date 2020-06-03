(* BMC-2 Abstract Syntax *)
(*author: Yu-Yang Lin, date: 2020*)
open Format
open Lexing

type pos_range = (Lexing.position * Lexing.position)
exception SyntaxError of string * pos_range
exception ParseError of string * pos_range
let line_sprintf p1 p2 =
  sprintf "(line %d , col %d) to (line %d , col %d)"
    (p1.pos_lnum) (p1.pos_cnum - p1.pos_bol)
    (p2.pos_lnum) (p2.pos_cnum - p2.pos_bol)

type kast = C of (string * kast list) 
          | H of (string * kast list) 
          | T of (string * kast list)
          | S of string
          | D of string

let rec string_of_kast kast =
  match kast with
  | C(s , ks) -> sprintf "%s(%s)" s (string_of_kast_list ks)
  | H(s , ks) -> sprintf "%s(%s)" s (string_of_kast_list ks)
  | T(s , ks) -> sprintf "%s(%s)" s (string_of_kast_list ks)
  | S(s)      -> s
  | D(s)      -> s
and string_of_kast_list ks =
  match ks with
  | [] -> ""
  | k::[] -> string_of_kast k
  | k::ks -> sprintf "%s,%s" (string_of_kast k) (string_of_kast_list ks)
