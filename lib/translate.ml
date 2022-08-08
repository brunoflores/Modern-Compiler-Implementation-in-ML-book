module type S = sig
  type exp = unit * Tiger.pos option
  type level

  val outermost : level

  type access

  val new_level : level -> Temp.label -> bool list -> level
  val formals : level -> access list
  val alloc_local : unit -> access
end

module Make (Frame : Frame.S) : S = struct
  type exp = unit * Tiger.pos option
  type level = unit
  type access = level * Frame.access

  let outermost : level = ()

  let new_level (_ : level) (_ : Temp.label) (_ : bool list) : level =
    failwith "not implemented"

  let formals (_ : level) : access list = failwith "not implemented"

  (* let alloc_local (_ : level) (_ : bool) : access = failwith "not implemented" *)
  let alloc_local (_ : unit) : access = failwith "not implemented"
end
