{
open Lexing
open Token

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let int = digit digit*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "/*"    { eat_comment lexbuf }
  | "let"   { LET }
  | "type"  { TYPE }
  | "of"    { OF }
  | "var"   { VAR }
  | "in"    { IN }
  | "end"   { END }
  | ":="    { ASSIGN }
  | "="     { EQ }
  | ":"     { COLON }
  | "["     { LBRACK }
  | "]"     { RBRACK }
  | id      { ID (Lexing.lexeme lexbuf) }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF }
and eat_comment =
  parse
  | "*/" { read lexbuf }
  | eof  { raise (SyntaxError "Comment is not terminated") }
  | _    { eat_comment lexbuf }
