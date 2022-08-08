(** An abstract representation of stack frames.
    Implementation of {!Frame.S} is specific to a target machine. *)

module type S = sig
  type frame
  (** The [frame] type holds information about formal parameters and local
      variables allocated in this frame. This is an abstract data type. *)

  type access
  (** The [access] type describes formals and locals that may be in the
      frame or in registers. This is an abstract data type, so its
      implementation is visible only inside the implementation. *)

  val new_frame : Temp.label -> bool list -> frame
  (** To make a new frame for a function [f] with [k] formal parameters,
      call [new_frame f l], where [l] is a list of [k] booleans: [true] for
      each parameter that escapes and [false] for each parameter that
      does not. The result will be a frame object. *)

  val name : frame -> Temp.label

  val formals : frame -> access list
  (** The [formals] interface function extracts a list of [k] "accesses"
      denoting the locations where the formal parameters will be kept at
      runtime, as seen from inside the callee. *)

  val alloc_local : frame -> bool -> access
  (** To allocate a new local variable in a frame [f], the semantic analysis
      phase calls [alloc_local f true].

      The boolean argument to [alloc_local] specifies whether the new variable
      escapes and needs to go in the frame; if it is [false], then the
      variable can be allocated in a register. *)
end
