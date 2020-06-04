%{
  open Kast
  open Format
  open Lexing
%}

/*=================*/
/*---- LEXEMES ----*/
/*=================*/

(*** KAST-TOKENS ***)
%token <string> TICK
%token <string> HASH
%token <string> CELL
%token <string> DOT
%token <string> STR
%token <string> KEY
%token OPEN_PAREN CLOSE_PAREN COMMA
%token EOF

/*=================================*/
/*---- START SYMBOLS AND TYPES ----*/
/*=================================*/

%start kast_toplevel
%type <Kast.kast> kast_toplevel
%type <Kast.kast> kast_middle
%type <Kast.kast list> kast_list
%%

/*=================*/
/*---- GRAMMAR ----*/
/*=================*/

(*** FILE ***)
kast_toplevel:
| HASH OPEN_PAREN kast_list CLOSE_PAREN EOF   { H($1 , $3) }
| TICK OPEN_PAREN kast_list CLOSE_PAREN EOF   { T($1 , $3) }
| CELL OPEN_PAREN kast_list CLOSE_PAREN EOF   { C($1 , $3) }

kast_middle:
| HASH OPEN_PAREN kast_list CLOSE_PAREN       { H($1 , $3) }
| TICK OPEN_PAREN kast_list CLOSE_PAREN       { T($1 , $3) }
| CELL OPEN_PAREN kast_list CLOSE_PAREN       { C($1 , $3) }
| STR                                         { S $1 }
| DOT                                         { D $1 }
| KEY                                         { K $1 }

kast_list:
| kast_middle                                 { [ $1 ] }
| kast_middle COMMA kast_list                 { $1 :: $3 }
