type exp =
  | Const of int
  | Name of Symbol.symbol
  | Temp of Temp.temp
  | Binop of binop * exp * exp
  | Mem of exp
  | Call of exp * exp list
  | Eseq of stm * exp
[@@deriving show]

and stm =
  | Move of exp * exp
  | Exp of exp
  | Jump of exp * Symbol.symbol list
  | Cjump of relop * exp * exp * Symbol.symbol * Symbol.symbol
  | Seq of stm list
  | Label of Symbol.symbol
[@@deriving show]

and binop =
  | Plus
  | Minus
  | Mul
  | Div
  | And
  | Ok
  | Xor
  | Lshift
  | Rshift
  | Arshift
[@@deriving show]

and relop = Eq | Ne | Lt | Gt | Le | Ge | Ult | Ule | Ugt | Uge
[@@deriving show]
