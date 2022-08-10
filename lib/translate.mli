(** Semantic translation.

    This module provides functions that are useful in producing intermediate
    representation from abstract syntax.

    Manages local variables and static function nesting for {!Semant}. *)

module type S = sig
  (** [exp] models the three kinds of abstract syntax expressions
      in the Tree IR *)
  type exp =
    | Ex of Tree.exp  (** Stands for an "expression" *)
    | Nx of Tree.stm  (** Stands for "no result" *)
    | Cx of (Temp.label * Temp.label -> Tree.stm)
        (** Stands for "conditional". Given a true-destination and a
            false-destination, it will make a statement that evaluates some
            conditionals and then jumps to one of the destinations
            (the statement will never "fall through"). *)

  type level
  (** For function static links. *)

  type access
  (* TODO document *)

  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : Temp.label * Temp.label -> Tree.stm

  val outermost : level
  (** The outermost level is the level within which the "main" program
      is nested. All "library" functions are declared at this outermost
      level. *)

  val new_level : level -> Temp.label -> bool list -> level
  (** Create a new level enclosing the given level. *)

  val formals : level -> access list
  (* TODO document *)

  val alloc_local : level -> bool -> access
end

(** Functor interface. *)
module Make (Frame : Frame.S) : S
