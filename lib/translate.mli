(** Semantic translation.

    This module provides functions that are useful in producing intermediate
    representation from abstract syntax.

    Manages local variables and static function nesting for {!Semant}. *)

module type S = sig
  (** Abstract type to stand as the interface between the Semant and Translate
      modules. *)
  type exp = Ex of Tree.exp [@@deriving show]

  type level
  (** For function static links. *)

  type access

  val outermost : level
  (** The outermost level is the level within which the "main" program
      is nested. All "library" functions are declared at this outermost
      level. *)

  val new_level : level -> Temp.label -> bool list -> level
  (** Create a new level enclosing the given level. *)

  val formals : level -> access list
  val alloc_local : level -> bool -> access
  val simple_var : access * level -> exp
end

(** Functor interface to abstract over machine-dependent Frame
    implementations. *)
module Make (_ : Frame.S) : S
