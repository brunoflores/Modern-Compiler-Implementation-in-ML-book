%{
  (* The AST. *)
  open Tiger

  let pos_of_lexing_position (pos : Lexing.position) : pos =
    { pos_fname = pos.pos_fname;
      pos_lnum = pos.pos_lnum;
      pos_bol = pos.pos_bol;
      pos_cnum = pos.pos_cnum }
%}

%token <Token.id> ID
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

%left before_BINOPS
%left AND OR
%left EQ NEQ TIMES MINUS PLUS GT
%left ID
%left LPAREN
%left DOT

%start <exp option> prog

%%

prog:
  | v = entryexpr; EOF { Some v }
  | EOF { None }

entryexpr:
  | x = openexpr { x }
  | x = closedexpr { x }

expr:
  | x = constant { x }
  | x = lvalue { VarExp x }
  | e = binop { e }
  | e = arrexp { e }
  | LPAREN; RPAREN { NilExp }
  | LPAREN; seq = exprseq+; RPAREN { SeqExp seq }

  | lvalue = lvalue; ASSIGN; e = expr
    { AssignExp {
        var = lvalue;
        exp = e;
        pos = (pos_of_lexing_position $startpos) } }
    %prec before_BINOPS

  | id = ID; LBRACE; fields = fields*; RBRACE
    { RecordExp {
        fields = fields;
        typ = id;
        pos = (pos_of_lexing_position $startpos) }  }

  | LET; decs = dec*; IN; seq = exprseq+; END
    { LetExp {
        decs = decs;
        body = SeqExp seq;
        pos = (pos_of_lexing_position $startpos) } }

  | id = ID; LPAREN; l = exprlist*; RPAREN
    { CallExp {
        func = id;
        args = l;
        pos = (pos_of_lexing_position $startpos) } }

  | WHILE; test = expr; DO; body = expr
    { WhileExp {
        test = test;
        body = body;
        pos = (pos_of_lexing_position $startpos) } }
    %prec before_BINOPS

arrexp:
  | id = ID; LBRACK; size = expr; RBRACK; OF; e = expr
    { ArrayExp {
        typ = id;
        size = size;
        init = e;
        pos = (pos_of_lexing_position $startpos) } }
    %prec before_BINOPS

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
        var = id;
        lo = lo;
        hi = hi;
        body = body;
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
    %prec EQ

  | left = expr; NEQ; right = expr
    { OpExp {
        left = left;
        oper = NeqOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }
    %prec NEQ

  | left = expr; TIMES; right = expr
    { OpExp {
        left = left;
        oper = TimesOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }
    %prec TIMES

  | left = expr; MINUS; right = expr
    { OpExp {
        left = left;
        oper = MinusOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }
    %prec MINUS

  | left = expr; PLUS; right = expr
    { OpExp {
        left = left;
        oper = PlusOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }
    %prec PLUS

  | left = expr; GT; right = expr
    { OpExp {
        left = left;
        oper = GtOp;
        right = right;
        pos = (pos_of_lexing_position $startpos) } }
    %prec GT

  | left = expr; AND; right = expr
    { IfExp {
        test = left;
        then' = right;
        else' = Some (IntExp 0);
        pos = (pos_of_lexing_position $startpos) } }
    %prec AND

  | left = expr; OR; right = expr
    { IfExp {
        test = left;
        then' = IntExp 1;
        else' = Some right;
        pos = (pos_of_lexing_position $startpos) } }
    %prec OR

exprlist:
  | e = entryexpr; { (e, (pos_of_lexing_position $startpos)) }
  | e = entryexpr; COMMA { (e, (pos_of_lexing_position $startpos)) }

exprseq:
  | e = entryexpr; { (e, (pos_of_lexing_position $startpos(e))) }
  | e = entryexpr; SEMICOLON { (e, (pos_of_lexing_position $startpos(e))) }

lvalue:
  | id = ID { SimpleVar (id, (pos_of_lexing_position $startpos)) }

  | var = lvalue; DOT; symbol = lvalue
    { FieldVar (var, symbol, (pos_of_lexing_position $startpos)) }

  | var = ID; LBRACK; e = expr; RBRACK
    { SubscriptVar (
        SimpleVar (var, (pos_of_lexing_position ($startpos(var)))),
        e,
        (pos_of_lexing_position $startpos)) }

dec:
  | x = vardec { x }
  | x = tydecs { x }
  | x = fundecs { x }

fundecs:
  | FUNCTION; l = fundec+ { FunctionDec l }

fundec:
  | id = ID; LPAREN; params = separated_list(COMMA, tyfield); RPAREN;
    result = resultopt?; EQ; body = entryexpr
    { { name = id;
        params = params;
        result = result;
        body = body;
        pos = (pos_of_lexing_position $startpos) } }

resultopt:
  | COLON; id = ID { (id, (pos_of_lexing_position $startpos)) }

vardec:
  | VAR; id = ID; COLON; tid = ID; ASSIGN; e = expr
    { VarDec {
        name = id;
        typ = Some (tid, (pos_of_lexing_position $startpos(tid)));
        init = e;
        pos = (pos_of_lexing_position $startpos) } }

  | VAR; id = ID; ASSIGN; e = expr
    { VarDec {
        name = id;
        typ = None;
        init = e;
        pos = (pos_of_lexing_position $startpos) } }

tydecs:
  | TYPE; l = tydec+ { TypeDec l }

tydec:
  | id = ID; EQ; tid = ID
    { { tydec_name = id;
        ty = NameTy (tid, (pos_of_lexing_position $startpos(tid)));
        tydec_pos = (pos_of_lexing_position $startpos) } }

  | id = ID; EQ; ARRAY; OF; tid = ID
    { { tydec_name = id;
        ty = (ArrayTy (tid, (pos_of_lexing_position $startpos(tid))));
        tydec_pos = (pos_of_lexing_position $startpos) } }

  | id = ID; EQ; LBRACE; fields = separated_list(COMMA, tyfield); RBRACE;
    { { tydec_name = id;
        ty = (RecordTy fields);
        tydec_pos = (pos_of_lexing_position $startpos) } }

tyfield:
  | id = ID; COLON; tid = ID
    { { field_name = id;
        typ = tid;
        field_pos = (pos_of_lexing_position $startpos) } }

fields:
  | x = field { x }
  | x = field; COMMA { x }

field:
  | id = ID; EQ; e = expr { ( id, e, (pos_of_lexing_position $startpos)) }
