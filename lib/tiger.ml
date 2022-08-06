(** The abstract syntax. *)

type pos = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving show]

type symbol = Symbol.symbol [@@deriving show]

type exp =
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | VarExp of var
  | CallExp of { func : symbol; args : (exp * pos) list; pos : pos }
  | OpExp of { left : exp; oper : oper; right : exp; pos : pos }
  | RecordExp of { fields : (symbol * exp * pos) list; typ : symbol; pos : pos }
  | SeqExp of (exp * pos) * (exp * pos) list  (** One or more expressions *)
  | AssignExp of { var : var; exp : exp; pos : pos }
  | IfExp of { test : exp; then' : exp; else' : exp option; pos : pos }
  | WhileExp of { test : exp; body : exp; pos : pos }
  | ForExp of {
      var : symbol;
      escape : bool ref;
      lo : exp;
      hi : exp;
      body : exp;
      pos : pos;
    }
  | BreakExp of pos
  | LetExp of { decs : dec list; body : exp; pos : pos }
  | ArrayExp of { typ : symbol; size : exp; init : exp; pos : pos }
[@@deriving show]

and var =
  | SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos
  | SubscriptVar of var * exp * pos

and dec =
  (* Why is FunctionDec a list?
     From the book, page 97:
       "The Tiger language treats adjacent function declarations as (possibly)
        mutually recursive. The FunctionDec constructor of the abstract
        syntax takes a list of function declarations, not just a single
        function. The intent is that this list is a maximal consecutive
        sequence of function declarations. Thus, functions declared by the
        same FunctionDec can be mutually recursive." *)
  | FunctionDec of functiondec list
  | VarDec of {
      name : symbol;
      escape : bool ref;
      typ : (symbol * pos) option;
      init : exp;
      pos : pos;
    }
  (* Why is TypeDec a list? See FunctionDec above. *)
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

and tydec = { tydec_name : symbol; ty : ty; tydec_pos : pos }

and field = {
  field_name : symbol;
  escape : bool ref;
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
