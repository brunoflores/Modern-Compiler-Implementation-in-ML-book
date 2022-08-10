module type S = sig
  type exp =
    | Ex of Tree.exp
    | Nx of Tree.stm
    | Cx of (Temp.label * Temp.label -> Tree.stm)

  type level
  type access

  val unEx : exp -> Tree.exp
  val unNx : exp -> Tree.stm
  val unCx : Temp.label * Temp.label -> Tree.stm
  val outermost : level
  val new_level : level -> Temp.label -> bool list -> level
  val formals : level -> access list
  val alloc_local : level -> bool -> access
end

module Make (Frame : Frame.S) : S = struct
  type exp =
    | Ex of Tree.exp
    | Nx of Tree.stm
    | Cx of (Temp.label * Temp.label -> Tree.stm)

  type level = unit
  type access = level * Frame.access

  let unEx _exp = failwith "not implemented"
  let unNx _exp = failwith "not implemented"
  let unCx (_true, _false) = failwith "not implemented"
  let outermost : level = ()

  let new_level (_ : level) (_ : Temp.label) (_ : bool list) : level =
    failwith "not implemented"

  let formals (_ : level) : access list = failwith "not implemented"
  let alloc_local (_ : level) (_ : bool) : access = failwith "not implemented"
end
