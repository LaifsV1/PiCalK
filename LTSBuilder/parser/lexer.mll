{
  open Kast
  open Parser
  open Lexing

  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
      }
      
  let lex_failure (msg : string) (pos1 : position) (pos2 : position) =
    SyntaxError ((Printf.sprintf "error lexing: %s" msg),(pos1,pos2))
}

let str  = '"' [' ''!''#'-'~']+ '"'
let cell = "`<" ['A'-'Z''a'-'z']+ ">`"
let tick = '`' [' '-'_''a'-'~']+ '`'
let hash = '#' ['A'-'Z''a'-'z''0'-'9''_''\'']+
let dot  = '.' ['A'-'Z''a'-'z''0'-'9''_''\'''>''<''['']''.''|']+
let key  = ['A'-'Z''a'-'z''0'-'9''_''\'''>''<''['']''.''|']+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white           { read lexbuf }
  | newline         { next_line lexbuf; read lexbuf }
  | '('             { OPEN_PAREN }
  | ')'             { CLOSE_PAREN }
  | ','             { COMMA }
  | str  as x       { STR x }
  | cell as x       { CELL x }  
  | tick as x       { TICK x }
  | hash as x       { HASH x }
  | dot  as x       { DOT x }
  | key  as x       { KEY x }
  | eof             { EOF }
  | _               { raise (lex_failure ("unknown symbol '"^(lexeme lexbuf)^"'") (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)) }
{
}
