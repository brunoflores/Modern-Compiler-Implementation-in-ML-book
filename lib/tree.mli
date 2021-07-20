(** Intermediate representation (IR) as expression trees. *)

type exp =
  | CONST of int  (** The integer constant *)
  | NAME of Symbol.symbol  (** Symbolic constant. See {!section: name}. *)
  | TEMP of Temp.temp  (** Temporary. See {!section: temp}. *)
  | BINOP of binop * exp * exp  (** Binary operator. See {!section: binop}. *)
  | MEM of exp  (** See {!section: mem}. *)
  | CALL of exp * exp list  (** A procedure call. See {!section: call}. *)
  | ESEQ of stm * exp  (** See {!section: eseq}. *)

(** The statements ({!stm}) perform side effects and control flow. *)
and stm =
  | MOVE of exp * exp  (** See {!section: move}. *)
  | EXP of exp  (** See {!section: exp}.*)
  | JUMP of exp * Symbol.symbol list  (** See {!section: jump}. *)
  | CJUMP of relop * exp * exp * Symbol.symbol * Symbol.symbol
      (** See {!section: cjump}. *)
  | SEQ of stm * stm  (** See {!section: seq}. *)
  | LABEL of Symbol.symbol  (** See {!section: label}. *)

and binop =
  | PLUS  (** Integer arithmetic. *)
  | MINUS  (** Integer arithmetic. *)
  | MUL  (** Integer arithmetic. *)
  | DIV  (** Integer arithmetic. *)
  | AND  (** Bitwise logical. *)
  | OR  (** Bitwise logical. *)
  | XOR  (** Bitwise logical. *)
  | LSHIFT  (** Integer logical shift. *)
  | RSHIFT  (** Integer logical shift. *)
  | ARSHIFT  (** Integer arithmetic right-shift. *)

and relop =
  | EQ  (** Integer equality (signed or unsigned). *)
  | NE  (** Integer nonequality (signed or unsigned). *)
  | LT  (** Signed integer inequality. *)
  | GT  (** Signed integer inequality. *)
  | LE  (** Signed integer inequality. *)
  | GE  (** Signed integer inequality. *)
  | ULT  (** Unsigned integer inequality. *)
  | ULE  (** Unsigned integer inequality. *)
  | UGT  (** Unsigned integer inequality. *)
  | UGE  (** Unsigned integer inequality. *)

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

    When [MEM] is used as the left child of a [MOVE], it means "store",
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
    [SEQ(s1, s2)] means statement {i s1} followed by {i s2}.
*)

(** {1:label LABEL}
    {v
    LABEL(n)
    v}
    Define the constant value of name {i n} to be the current machine code
    address. This is like a label definition in assembly language.

    The value [NAME(n)] may be the target of jumps, calls, etc.
*)
