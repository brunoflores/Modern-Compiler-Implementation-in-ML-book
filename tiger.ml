type pos = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving show]

type symbol = string [@@deriving show]

type exp =
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | VarExp of var
  | CallExp of { func : symbol; args : (exp * pos) list; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | RecordExp of { fields : (symbol * exp * pos) list; typ : symbol; pos : pos }
  | SeqExp of (exp * pos) list
  | AssignExp of { var : var; exp : exp; pos : pos }
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { test : exp; body : exp; pos : pos }
  | ForExp of {
      var : symbol;
      (* escape : bool ref; *)
      lo : exp;
      hi : exp;
      body : exp;
      pos : pos;
    }
  | BreakExp of pos
  | LetExp of { decs : dec list; body : exp; pos : pos }
  | ArrayExp of { typ : symbol; size : exp; init : exp; pos : pos }

and var =
  | SimpleVar of symbol * pos
  | FieldVar of var * var * pos
  | SubscriptVar of var * exp * pos

and dec =
  | FunctionDec of functiondec list
  | VarDec of {
      name : symbol;
      (* escape : bool ref; *)
      typ : (symbol * pos) option;
      init : exp;
      pos : pos;
    }
  | TypeDec of tydec list

and ty =
  | NameTy of symbol * pos
  | RecordTy of field list
  | ArrayTy of symbol * pos

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  | AndOp

and tydec = { tydec_name : symbol; ty : ty; tydec_pos : pos }

and field = {
  field_name : symbol;
  (* escape : bool ref; *)
  typ : symbol;
  field_pos : pos;
}

and functiondec = {
  name : symbol;
  params : field list;
  result : (symbol * pos) option;
  body : exp;
  pos : pos;
}
[@@deriving show]