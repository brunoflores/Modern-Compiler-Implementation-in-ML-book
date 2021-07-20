module Make (Frame : Frame.I) : Translate.I = struct
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
