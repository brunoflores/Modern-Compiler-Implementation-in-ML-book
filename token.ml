type id = string

type token =
  (* identifier *)
  | ID of id
  (* literal integer *)
  | INT of int
  (* literal string *)
  | STRING of string
  (* while *)
  | WHILE
  (* for *)
  | FOR
  (* to *)
  | TO
  (* break *)
  (* | BREAK *)
  (* let *)
  | LET
  (* in *)
  | IN
  (* end *)
  | END
  (* function *)
  | FUNCTION
  (* var *)
  | VAR
  (* type *)
  | TYPE
  (* array *)
  | ARRAY
  (* if *)
  | IF
  (* then *)
  | THEN
  (* else *)
  | ELSE
  (* do *)
  | DO
  (* of *)
  | OF
  (* nil *)
  (* | NIL *)
  (* , *)
  | COMMA
  (* : *)
  | COLON
  (* ; *)
  | SEMICOLON
  (* ( *)
  | LPAREN
  (* ) *)
  | RPAREN
  (* [ *)
  | LBRACK
  (* ] *)
  | RBRACK
  (* { *)
  | LBRACE
  (* } *)
  | RBRACE
  (* . *)
  | DOT
  (* + *)
  | PLUS
  (* - *)
  | MINUS
  (* * *)
  | TIMES
  (* / *)
  (* | DIVIDE *)
  (* = *)
  | EQ
  (* < *)
  (* | LT *)
  (* > *)
  | GT
  (* <> *)
  | NEQ
  (* <= *)
  (* | LE *)
  (* >= *)
  (* | GE *)
  (* & *)
  | AND
  (* | *)
  | OR
  (* := *)
  | ASSIGN
  (* end of file *)
  | EOF

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
