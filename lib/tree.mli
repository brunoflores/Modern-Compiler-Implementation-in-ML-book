(** Intermediate representation (IR) as expression trees. *)

type exp =
  | Const of int  (** The integer constant *)
  | Name of Temp.label  (** Symbolic constant. See {!section: name}. *)
  | Temp of Temp.temp  (** Temporary. See {!section: temp}. *)
  | Binop of binop * exp * exp  (** Binary operator. See {!section: binop}. *)
  | Mem of exp  (** See {!section: mem}. *)
  | Call of exp * exp list  (** A procedure call. See {!section: call}. *)
  | Eseq of stm * exp  (** See {!section: eseq}. *)
[@@deriving show]

(** The statements ({!stm}) perform side effects and control flow. *)
and stm =
  | Move of exp * exp  (** See {!section: move}. *)
  | Exp of exp  (** See {!section: exp}.*)
  | Jump of exp * Symbol.symbol list  (** See {!section: jump}. *)
  | Cjump of relop * exp * exp * Symbol.symbol * Symbol.symbol
      (** See {!section: cjump}. *)
  | Seq of stm list  (** See {!section: seq}. *)
  | Label of Symbol.symbol  (** See {!section: label}. *)
[@@deriving show]

and binop =
  | Plus  (** Integer arithmetic. *)
  | Minus  (** Integer arithmetic. *)
  | Mul  (** Integer arithmetic. *)
  | Div  (** Integer arithmetic. *)
  | And  (** Bitwise logical. *)
  | Ok  (** Bitwise logical. *)
  | Xor  (** Bitwise logical. *)
  | Lshift  (** Integer logical shift. *)
  | Rshift  (** Integer logical shift. *)
  | Arshift  (** Integer arithmetic right-shift. *)
[@@deriving show]

and relop =
  | Eq  (** Integer equality (signed or unsigned). *)
  | Ne  (** Integer nonequality (signed or unsigned). *)
  | Lt  (** Signed integer inequality. *)
  | Gt  (** Signed integer inequality. *)
  | Le  (** Signed integer inequality. *)
  | Ge  (** Signed integer inequality. *)
  | Ult  (** Unsigned integer inequality. *)
  | Ule  (** Unsigned integer inequality. *)
  | Ugt  (** Unsigned integer inequality. *)
  | Uge  (** Unsigned integer inequality. *)
[@@deriving show]

(** {1:name NAME}
    Corresponds to an assembly language label.
*)

(** {1:temp TEMP}
    A temporary in the abstract machine is similar to a register in a
    real machine. However, the abstract machine has an infinite number of
    temporaries.
*)

(** {1:binop BINOP}
    [BINOP(o, e1, e2)] means the application of binary {i o} to
    operands {i e1} and {i e2}.

    Subexpression {i e1} is evaluated before {i e2}.
*)

(** {1:mem MEM}
    [MEM(e)]

    The contents of {i wordSize} bytes of memmory starting at address {i e}
    (where {i wordSize} is defined by an implementation of {!Frame}).

    When [MEM] is used as the left child of a [MOVE] it means "store",
    but anywhere else it means "fetch".
*)

(** {1:call CALL}
    [CALL(f, l)] means the application of function {i f} to argument list {i l}.

    {ol
    {- The subexpression {i f} is evaluated before the arguments;}
    {- The arguments are evaluated from left to right.}
    }
*)

(** {1:eseq ESEQ}
    [ESEQ(s, e)] means the statement {i s} is evaluated for side effects,
    then {i e} is evaluated for a result.
*)

(** {1:move MOVE}
    Evaluate {i e} and move it into temporary {i t}:
    {v
    MOVE(TEMP t, e)
    v}

    Evaluate {i e1}, yielding address {i a}. Then evaluate {i e2}, and store
    the result into {i wordSize} bytes of memory starting at {i a}:
    {v
    MOVE(MEM(e1), e2)
    v}
*)

(** {1:exp EXP}
    Evaluate {i e} and discard the result:
    {v
    EXP(e)
    v}
*)

(** {1:jump JUMP}
    {v
    JUMP(e, labels)
    v}

    Transfer control to address {i e}. The destination {i e} may be a literal
    label, as in [NAME(l)], or it may be an address calculated by any other
    kind of expression.

    The list of labels [labels] specifies all the possible locations
    that the expression {i e} can evaluate to.

    The common case of jumping to a known label [l] is written as
    [JUMP(NAME l, [l])].
*)

(** {1:cjump CJUMP}
    {v
    CJUMP(o, e1, e2, t, f)
    v}

    Evaluate {i e1}, {i e2} in that order, yielding values {i a}, {i b}.
    Then compare {i a}, {i b} using the relational operator {i o}.
    If the result is [true], jump to [t]; otherwise jump to [f].
*)

(** {1:seq SEQ}
    {v
    SEQ(statements)
    v}

    Execute {i statements} in order.
*)

(** {1:label LABEL}
    {v
    LABEL(n)
    v}
    Define the constant value of name {i n} to be the current machine code
    address. This is like a label definition in assembly language.

    The value [NAME(n)] may be the target of jumps, calls, etc.
*)
