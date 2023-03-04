module type S = sig
  type exp =
    | Ex of Tree.exp
    | Nx of Tree.stm
    | Cx of (Temp.label * Temp.label -> Tree.stm)
  [@@deriving show]

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

  type level = Frame.frame
  type access = level * Frame.access

  let _unEx _exp = failwith "not implemented"
  let _unNx _exp = failwith "not implemented"
  let _unCx (_true, _false) = failwith "not implemented"
  let outermost : level = Frame.new_frame (Symbol.create "outermost") [ true ]

  let new_level (_level : level) (label : Temp.label) (escape : bool list) :
      level =
    Frame.new_frame label escape

  let formals (level : level) : access list =
    List.map (fun access -> (level, access)) (Frame.formals level)

  let alloc_local (level : level) (escape : bool) : access =
    (level, Frame.alloc_local level escape)

  let simple_var (_access, _level) = Ex (Tree.Const 0)
end
