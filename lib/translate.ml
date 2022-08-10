module type S = sig
  type exp [@@deriving show]
  type level
  type access

  val outermost : level
  val new_level : level -> Temp.label -> bool list -> level
  val formals : level -> access list
  val alloc_local : level -> bool -> access
  val simple_var : access * level -> exp
end

module Make (Frame : Frame.S) : S = struct
  type exp =
    | Ex of Tree.exp  (** Stands for an "expression" *)
    | Nx of Tree.stm  (** Stands for "no result" *)
    | Cx of (Temp.label * Temp.label -> Tree.stm)
        (** Stands for "conditional".
            Given a true-destination and a false-destination, it will make a
            statement that evaluates some conditionals and then jumps to one
            of the destinations (the statement will never "fall through"). *)
  [@@deriving show]

  type level = unit
  type access = level * Frame.access

  let _unEx _exp = failwith "not implemented"
  let _unNx _exp = failwith "not implemented"
  let _unCx (_true, _false) = failwith "not implemented"
  let outermost : level = ()

  let new_level (_ : level) (_ : Temp.label) (_ : bool list) : level =
    failwith "not implemented"

  let formals (_ : level) : access list = failwith "not implemented"
  let alloc_local (_ : level) (_ : bool) : access = failwith "not implemented"
  let simple_var (_access, _level) = Ex (Tree.Const 0)
end
