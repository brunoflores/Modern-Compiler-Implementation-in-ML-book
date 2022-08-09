(** Semantic translation.

    This module provides functions that are useful in producing intermediate
    representation from abstract syntax.

    Manages local variables and static function nesting for {!Semant}. *)

module type S = sig
  type exp = unit * Tiger.pos option
  (** The translation of an expression into intermediate code. *)

  type level
  (** For function static links. *)

  val outermost : level
  (** The outermost level is the level within which the "main" program
      is nested. All "library" functions are declared at this outermost
      level. *)

  type access

  val new_level : level -> Temp.label -> bool list -> level
  (** Create a new level enclosing the given level. *)

  val formals : level -> access list
  (** TODO document*)

  (* val alloc_local : level -> bool -> access *)
  val alloc_local : unit -> access
end

(** Functor interface. *)
module Make (Frame : Frame.S) : S
