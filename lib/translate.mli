(** Semantic translation.

    This module provides functions that are useful in producing intermediate
    representation from abstract syntax.

    Manages local variables and static function nesting for {!Semant}. *)

module type S = sig
  type exp [@@deriving show]
  (** Abstract type to stand as the interface between the Semant and Translate
      modules. *)

  type level
  (** For function static links. *)

  type access
  type frag [@@deriving show]

  val outermost : level
  (** The outermost level is the level within which the "main" program
      is nested. All "library" functions are declared at this outermost
      level. *)

  val new_level : level -> Temp.label -> bool list -> level
  (** Create a new level enclosing the given level. *)

  val formals : level -> access list
  val alloc_local : level -> bool -> access

  val simple_var : access * level -> exp
  (** Produce a chain of MEM and + nodes to fetch static links for all frames
      between the level of use (the [level] passed to this function) and the
      level of definition (the [level] within the variable's [access]). *)

  val simple_int : int -> exp
  val simple_op : Tiger.oper -> exp -> exp -> exp
  val sequence : exp -> exp -> exp
  val procEntryExit : level * exp -> unit
  val get_result : unit -> frag list
end

(** Functor interface to abstract over machine-dependent Frame
    implementations. *)
module Make (Frame : Frame.S) : S with type frag = Frame.frag
