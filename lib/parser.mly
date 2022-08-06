%{
  (* The AST. *)
  open Tiger

  let pos_of_lexing_position (pos : Lexing.position) : pos =
    { pos_fname = pos.pos_fname;
      pos_lnum = pos.pos_lnum;
      pos_bol = pos.pos_bol;
      pos_cnum = pos.pos_cnum }
%}

%token <string> ID
%token <int> INT
%token <string> STRING
%token END
%token LET
%token IN
%token ASSIGN
%token VAR
%token EQ
%token TIMES
%token MINUS
%token PLUS
%token ARRAY
%token TYPE
%token COLON
%token SEMICOLON
%token RBRACK
%token LBRACK
%token RBRACE
%token LBRACE
%token RPAREN
%token LPAREN
%token GT
%token COMMA
%token OF
%token FUNCTION
%token DOT
%token THEN
%token WHILE
%token FOR
%token DO
%token TO
%token IF
%token ELSE
%token NEQ
%token AND
%token OR
%token EOF

(* Page 517: Precedence of operators
   Unary minus (negation) has the highest precedence.
   Then operators *, / have the next highest precedence,
   followed by +, -, then by =, <>, >, <, >=, <=, then by &, then by |. *)

(* Page 518, Associativity of operators
   The operators *, /, +, - are all left-associative.
   The comparison operators do not associate, so a=b=c is not a
   legal expression, although a=(b=c) is. *)

%nonassoc ASSIGN
%nonassoc DO
%nonassoc OF
%left OR
%left AND
%nonassoc EQ NEQ GT
%left MINUS PLUS
%left TIMES
%nonassoc ID
%left LBRACK
%left LPAREN

%start <exp option> prog

%%

prog:
  | v = entryexpr; EOF { Some v }
  | EOF { None }

entryexpr:
  | x = openexpr { x }
  | x = closedexpr { x }

// For when ID[expr] is found in the right-hand side.
// This happens in array creation, when expr is the number of elements.
// "Array creation" (page 518).
%inline exprorarr:
  | e = expr { e }
  | id = ID; LBRACK; size = expr; RBRACK; OF; e = expr
    { ArrayExp {
        typ = Symbol.create id;
        size = size;
        init = e;
        pos = (pos_of_lexing_position $startpos) } }

expr:
  | x = constant { x }
  | x = lvalue { VarExp x }
  | e = binop { e }
  | LPAREN; RPAREN { NilExp }
  | LPAREN; seq = exprseq+; RPAREN
    { let seq_head, seq_tail = match seq with
      | [] -> failwith "found empty sequence"
      | hd :: [] -> (hd, [])
      | hd :: tl -> (hd, tl)
      in SeqExp (seq_head, seq_tail) }

  | left = expr; AND; right = expr
    { IfExp {
        test = left;
        then' = right;
        else' = Some (IntExp 0);
        pos = (pos_of_lexing_position $startpos) } }

  | left = expr; OR; right = expr
    { IfExp {
        test = left;
        then' = IntExp 1;
        else' = Some right;
        pos = (pos_of_lexing_position $startpos) } }

  | lvalue = lvalue; ASSIGN; e = exprorarr
    { AssignExp {
        var = lvalue;
        exp = e;
        pos = (pos_of_lexing_position $startpos) } }

  | id = ID; LBRACE; fields = fields*; RBRACE
    { RecordExp {
        fields = fields;
        typ = Symbol.create id;
        pos = (pos_of_lexing_position $startpos) }  }

  | LET; decs = dec*; IN; seq = exprseq+; END
    { let seq_head, seq_tail = match seq with
      | [] -> failwith "found empty sequence"
      | hd :: [] -> (hd, [])
      | hd :: tl -> (hd, tl)
      in LetExp { decs = decs;
                  body = SeqExp (seq_head, seq_tail);
                  pos = (pos_of_lexing_position $startpos) } }

  | id = ID; LPAREN; l = exprlist*; RPAREN
    { CallExp {
        func = Symbol.create id;
        args = l;
        pos = (pos_of_lexing_position $startpos) } }

  | WHILE; test = expr; DO; body = expr
    { WhileExp {
        test = test;
        body = body;
        pos = (pos_of_lexing_position $startpos) } }

constant:
  | x = INT { IntExp x }
  | x = STRING { StringExp (x, (pos_of_lexing_position $startpos)) }

openexpr:
  | IF; test = entryexpr; THEN; thenbody = expr
    { IfExp {
        test = test;
        then' = thenbody;
        else' = None;
        pos = (pos_of_lexing_position $startpos) } }

  | IF; test = entryexpr; THEN; thenbody = openexpr
    { IfExp {
        test = test;
        then' = thenbody;
        else' = None;
        pos = (pos_of_lexing_position $startpos) } }

  | IF; test = entryexpr; THEN; thenbody = closedexpr;
    ELSE; elsebody = openexpr
    { IfExp {
        test = test;
        then' = thenbody;
        else' = Some elsebody;
        pos = (pos_of_lexing_position $startpos) } }

  | FOR; id = ID; ASSIGN; lo = expr; TO; hi = expr; DO; body = entryexpr
    { ForExp {
        var = Symbol.create id;
        lo = lo;
        hi = hi;
        body = body;
        escape = ref true;
        pos = (pos_of_lexing_position $startpos) } }

closedexpr:
  | e = expr { e }
  | IF; test = entryexpr; THEN; thenbody = closedexpr;
    ELSE; elsebody = closedexpr
    { IfExp {
        test = test;
        then' = thenbody;
        else' = Some elsebody;
        pos = (pos_of_lexing_position $startpos) } }

binop:
  | left = expr; EQ; right = expr
    { OpExp {
        left = left;
        oper = EqOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }

  | left = expr; NEQ; right = expr
    { OpExp {
        left = left;
        oper = NeqOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }

  | left = expr; TIMES; right = expr
    { OpExp {
        left = left;
        oper = TimesOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }

  | left = expr; MINUS; right = expr
    { OpExp {
        left = left;
        oper = MinusOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }

  | left = expr; PLUS; right = expr
    { OpExp {
        left = left;
        oper = PlusOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }

  | left = expr; GT; right = expr
    { OpExp {
        left = left;
        oper = GtOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }

 exprlist:
  | e = entryexpr; { (e, (pos_of_lexing_position $startpos)) }
  | e = entryexpr; COMMA { (e, (pos_of_lexing_position $startpos)) }

exprseq:
  | e = entryexpr; { (e, (pos_of_lexing_position $startpos(e))) }
  | e = entryexpr; SEMICOLON { (e, (pos_of_lexing_position $startpos(e))) }

lvalue:
  | id = ID { SimpleVar (Symbol.create id, (pos_of_lexing_position $startpos)) }

  | var = lvalue; DOT; symbol = ID
    { FieldVar (var, Symbol.create symbol, (pos_of_lexing_position $startpos)) }

  | var = lvalue; LBRACK; e = expr; RBRACK
    { SubscriptVar (var, e, (pos_of_lexing_position $startpos)) }

dec:
  | x = vardec { x }
  | x = tydecs { x }
  | x = fundecs { x }

fundecs:
  | FUNCTION; l = fundec+ { FunctionDec l }

fundec:
  | id = ID; LPAREN; params = separated_list(COMMA, tyfield); RPAREN;
    result = resultopt?; EQ; body = entryexpr
    { { name = Symbol.create id;
        params = params;
        result = result;
        body = body;
        pos = (pos_of_lexing_position $startpos) } }

resultopt:
  | COLON; id = ID { (Symbol.create id, (pos_of_lexing_position $startpos)) }

vardec:
  | VAR; id = ID; COLON; tid = ID; ASSIGN; e = exprorarr
    { VarDec {
        name = Symbol.create id;
        typ = Some (Symbol.create tid, (pos_of_lexing_position $startpos(tid)));
        init = e;
        escape = ref true;
        pos = (pos_of_lexing_position $startpos) } }

  | VAR; id = ID; ASSIGN; e = exprorarr
    { VarDec {
        name = Symbol.create id;
        typ = None;
        init = e;
        escape = ref true;
        pos = (pos_of_lexing_position $startpos) } }

tydecs:
  | TYPE; l = tydec+ { TypeDec l }

tydec:
  | id = ID; EQ; tid = ID
    { { tydec_name = Symbol.create id;
        ty = NameTy (
          Symbol.create tid,
          (pos_of_lexing_position $startpos(tid)));
        tydec_pos = (pos_of_lexing_position $startpos) } }

  | id = ID; EQ; ARRAY; OF; tid = ID
    { { tydec_name = Symbol.create id;
        ty = (ArrayTy (
          Symbol.create tid,
          (pos_of_lexing_position $startpos(tid))));
        tydec_pos = (pos_of_lexing_position $startpos) } }

  | id = ID; EQ; LBRACE; fields = separated_list(COMMA, tyfield); RBRACE;
    { { tydec_name = Symbol.create id;
        ty = (RecordTy fields);
        tydec_pos = (pos_of_lexing_position $startpos) } }

tyfield:
  | id = ID; COLON; tid = ID
    { { field_name = Symbol.create id;
        typ = Symbol.create tid;
        escape = ref true;
        field_pos = (pos_of_lexing_position $startpos) } }

fields:
  | x = field { x }
  | x = field; COMMA { x }

field:
  | id = ID; EQ; e = exprorarr
    { (Symbol.create id, e, (pos_of_lexing_position $startpos)) }
