type id = string
(** The value of an identifier. *)

type token =
  | ID of id  (** identifier *)
  | INT of int  (** literal integer *)
  | STRING of string  (** literal string *)
  | WHILE  (** "while" *)
  | FOR  (** "for" *)
  | TO  (** "to" *)
  (* break *)
  (* | BREAK *)
  | LET  (** "let" *)
  | IN  (** "in" *)
  | END  (** "end" *)
  | FUNCTION  (** "function" *)
  | VAR  (** "var" *)
  | TYPE  (** "type" *)
  | ARRAY  (** "array" *)
  | IF  (** "if" *)
  | THEN  (** "then" *)
  | ELSE  (** "else" *)
  | DO  (** "do" *)
  | OF  (** "of" *)
  (* nil *)
  (* | NIL *)
  | COMMA  (** "," *)
  | COLON  (** ":" *)
  | SEMICOLON  (** ";" *)
  | LPAREN  (** "(" *)
  | RPAREN  (** ")" *)
  | LBRACK  (** "\[" *)
  | RBRACK  (** "\]" *)
  | LBRACE  (** "\{" *)
  | RBRACE  (** "\}" *)
  | DOT  (** "." *)
  | PLUS  (** "+" *)
  | MINUS  (** "-" *)
  | TIMES  (** "*" *)
  (* / *)
  (* | DIVIDE *)
  | EQ  (** "=" *)
  (* < *)
  (* | LT *)
  | GT  (** ">" *)
  | NEQ  (** "<>" *)
  (* <= *)
  (* | LE *)
  (* >= *)
  (* | GE *)
  | AND  (** "&" *)
  | OR  (** "|" *)
  | ASSIGN  (** ":=" *)
  | EOF  (** end of file *)

(** Convert a {!token} to string.
    This is valuable when printing tokens to the screen. *)
let string_of_token token =
  match token with
  | INT x -> "INT(" ^ string_of_int x ^ ")"
  | ID x -> "ID(" ^ x ^ ")"
  | STRING x -> "STRING(" ^ x ^ ")"
  | EOF -> "EOF"
  | LET -> "LET"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | TO -> "TO"
  (* | BREAK -> "BREAK" *)
  | IN -> "IN"
  | END -> "END"
  | FUNCTION -> "FUNCTION"
  | VAR -> "VAR"
  | TYPE -> "TYPE"
  | ARRAY -> "ARRAY"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | DO -> "DO"
  | OF -> "OF"
  (* | NIL -> "NIL" *)
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | DOT -> "DOT"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  (* | DIVIDE -> "DIVIDE" *)
  | EQ -> "EQ"
  (* | LT -> "LT" *)
  | GT -> "GT"
  | NEQ -> "NEQ"
  (* | LE -> "LE" *)
  (* | GE -> "GE" *)
  | AND -> "AND"
  | OR -> "OR"
  | ASSIGN -> "ASSIGN"
