module type S = sig
  type exp =
    | Ex of Tree.exp
    | Nx of Tree.stm
    | Cx of (t:Temp.label -> f:Temp.label -> Tree.stm)
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
    | Cx of (t:Temp.label -> f:Temp.label -> Tree.stm)
        (** Stands for "conditional".
          Given a true-destination and a false-destination, it will make a
          statement that evaluates some conditionals and then jumps to one
          of the destinations (the statement will never "fall through"). *)
  [@@deriving show]

  type level = Frame.frame
  type access = level * Frame.access

  let unEx = function
    | Ex e -> e
    | Nx s -> Tree.Eseq (s, Tree.Const 0)
    | Cx genstm ->
        (* Convert a conditional into a value expression.
           We invent a new temporary [r] and new labels [t] and [f].
           Then we make a Tree statement that moves 1 into [r]. and a conditional
           jump that implements the conditional. If the condition is false, then
           0 is moved into [r]; if true, then execution proceeds at [t] and the
           second move is skipped.
           The result is just the temporary [r] containing zero or one. *)
        let r = Temp.new_temp () in
        let t = Temp.new_label () in
        let f = Temp.new_label () in
        Tree.Eseq
          ( Tree.Seq
              [
                Tree.Move (Tree.Temp r, Tree.Const 1);
                genstm ~t ~f;
                Tree.Label f;
                Tree.Move (Tree.Temp r, Tree.Const 0);
                Tree.Label t;
              ],
            Tree.Temp r )

  let unNx = function
    | Ex e -> Tree.Exp e
    | Nx s -> s
    | Cx genstm ->
        let torf = Temp.new_label () in
        genstm ~t:torf ~f:torf

  let unCx (e : exp) : t:Temp.label -> f:Temp.label -> Tree.stm =
    match e with
    | Ex e ->
        fun ~t ~f ->
          let _ = t in
          let _ = f in
          Tree.Exp e
    | Nx s ->
        fun ~t ~f ->
          let _ = t in
          let _ = f in
          s
    | Cx genstm -> genstm

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
