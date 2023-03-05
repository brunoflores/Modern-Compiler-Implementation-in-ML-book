(** An abstract representation of stack frames.
    Implementations of {!Frame.S} are specific to a target machine. *)

module type S = sig
  type frame
  (** The [frame] type holds information about formal parameters and local
      variables allocated in this frame. *)

  type access
  (** The [access] type describes formals and locals that may be in the
      frame or in registers. *)

  val fp : Temp.temp
  (** Frame-pointer register. *)

  val word_size : int
  (** Natural word size. *)

  val exp : access -> Tree.exp -> Tree.exp
  (** Turn a [Frame.access] into the [Tree] expression.
      The [Tree.exp] argument is the address of the stack frame that the access
      lives in. When accessing from an inner-nested function, the frame address
      must be calculated following static links, and the result of this
      calculation is the [Tree.exp] argument to this function:

      * Own level: [TEMP(Frame.FP))]
      * Else: [MEM(+(CONST k_n, MEM(+(CONST k_n-1, ...
                                  MEM(+(CONST k_1, TEMP FP)) ...))))] *)

  val new_frame : Temp.label -> bool list -> frame
  (** To make a new frame for a function [f] with [k] formal parameters,
      call [new_frame f l], where [l] is a list of formals indicating whether
      the parameter escapes (needs to be kept in memory) or not.
      The result is a frame object. *)

  val name : frame -> Temp.label
  (** The frame name. *)

  val formals : frame -> access list
  (** The [formals] interface function extracts a list of [k] "accesses"
      denoting the locations where the formal parameters will be kept at
      runtime, as seen from inside the callee. *)

  val alloc_local : frame -> bool -> access
  (** To allocate a new local variable in a frame. *)
end

module Make : S
(** Functor interface. *)
