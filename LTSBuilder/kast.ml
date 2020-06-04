(* K Abstract Syntax *)
(*author: Yu-Yang Lin, date: 2020*)
open Format
open Lexing

(* for lexer and parser *)
type pos_range = (Lexing.position * Lexing.position)
exception SyntaxError of string * pos_range
exception ParseError of string * pos_range
let line_sprintf p1 p2 =
  sprintf "(line %d , col %d) to (line %d , col %d)"
    (p1.pos_lnum) (p1.pos_cnum - p1.pos_bol)
    (p2.pos_lnum) (p2.pos_cnum - p2.pos_bol)

(* kast type *)
type kast = C of (string * kast list) (* cell *)
          | H of (string * kast list) (* hash *) 
          | T of (string * kast list) (* tick *)
          | D of string               (* .type *)
          | S of string               (* string *)
          | K of string               (* keyword *)
