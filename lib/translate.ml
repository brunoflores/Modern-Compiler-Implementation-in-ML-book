module type S = sig
  type exp = unit * Tiger.pos option
  type level
  type access

  val outermost : level
  val new_level : level -> Temp.label -> bool list -> level
  val formals : level -> access list
  val alloc_local : level -> bool -> access
end

module Make (Frame : Frame.S) : S = struct
  type exp = unit * Tiger.pos option
  type level = unit
  type access = level * Frame.access

  let outermost : level = ()

  let new_level (_ : level) (_ : Temp.label) (_ : bool list) : level =
    failwith "not implemented"

  let formals (_ : level) : access list = failwith "not implemented"
  let alloc_local (_ : level) (_ : bool) : access = failwith "not implemented"
end
