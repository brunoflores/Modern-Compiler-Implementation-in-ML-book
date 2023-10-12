module type S = sig
  type exp [@@deriving show]
  type level
  type access
  type frag [@@deriving show]

  val outermost : level
  val new_level : level -> Temp.label -> bool list -> level
  val formals : level -> access list
  val alloc_local : level -> bool -> access
  val simple_var : access * level -> exp
  val simple_int : int -> exp
  val simple_op : Tiger.oper -> exp -> exp -> exp
  val sequence : exp -> exp -> exp
  val procEntryExit : level * exp -> unit
  val get_result : unit -> frag list
end

module Make (Frame : Frame.S) : S with type frag = Frame.frag = struct
  type exp =
    | Ex of Tree.exp  (** Stands for an "expression" *)
    | Nx of Tree.stm  (** Stands for "no result" *)
    | Cx of (t:Temp.label -> f:Temp.label -> Tree.stm)
        (** Stands for "conditional".
          Given a true-destination and a false-destination, it will make a
          statement that evaluates some conditionals and then jumps to one
            of the destinations (the statement will never "fall through"). *)
  [@@deriving show]

  type frag = Frame.frag [@@deriving show]

  let frags : frag list ref = ref []
  let _ = Nx (Tree.Seq [])

  let _ =
    Cx
      (fun ~t ~f ->
        let _ = t in
        let _ = f in
        Tree.Seq [])

  type level_id = unit ref
  type level = Bottom | Level of level * Frame.frame * level_id
  type access = level_id * Frame.access

  let unEx = function
    | Ex e -> e
    | Nx s -> Tree.Eseq (s, Tree.Const 0)
    | Cx genstm ->
        (* Convert a conditional into a value expression.

           We invent a new temporary [r] and new labels [t] and [f].  Then we
           make a Tree statement that moves 1 into [r]. and a conditional jump
           that implements the conditional. If the condition is false, then 0 is
           moved into [r]; if true, then execution proceeds at [t] and the
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

  let _unCx (e : exp) : t:Temp.label -> f:Temp.label -> Tree.stm =
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

  let outermost : level =
    (* let id = ref () in *)
    (* (Frame.new_frame (Symbol.create "outermost") [ true ], id) *)
    Bottom

  let new_level (parent : level) (label : Temp.label) (escape : bool list) :
      level =
    let id = ref () in
    (* Add static link - it escapes. *)
    let escape = true :: escape in
    Level (parent, Frame.new_frame label escape, id)

  let formals level : access list =
    match level with
    | Bottom -> failwith "called formals on Bottom"
    | Level (_parent, frame, id) ->
        (* Take the tail to discard the static link. *)
        let formals = List.tl (Frame.formals frame) in
        List.map (fun access -> (id, access)) formals

  let alloc_local level (escape : bool) : access =
    match level with
    | Bottom -> failwith "called alloc_local on Bottom"
    | Level (_parent, frame, id) -> (id, Frame.alloc_local frame escape)

  let simple_var ((level_id, frame_access), level) =
    let rec iter k = function
      | Bottom -> failwith "simple_var iter reached Bottom"
      | Level (parent, frame, id) ->
          (* Test for physical equality. *)
          if level_id == id then k
          else
            let k' = List.length (Frame.formals frame) in
            iter (k + k') parent
    in
    match level with
    | Bottom -> failwith "simple_var on Bottom"
    | Level (parent, frame, id) ->
        (* Test for physical equality. *)
        if level_id == id then Ex (Frame.exp frame_access (Tree.Temp Frame.fp))
        else
          let k = List.length (Frame.formals frame) in
          let k = iter k parent in
          Ex
            (Frame.exp frame_access
               (Tree.Mem
                  (Tree.Binop (Tree.Plus, Tree.Const k, Tree.Temp Frame.fp))))

  let simple_int i = Ex (Tree.Const i)

  let simple_op op left right =
    match op with
    | Tiger.PlusOp -> Ex (Tree.Binop (Tree.Plus, unEx left, unEx right))
    | _ -> failwith "not implemented"

  let sequence (e1 : exp) (e2 : exp) : exp = Ex (Tree.Eseq (unNx e1, unEx e2))

  let procEntryExit (level, body) =
    match level with
    | Bottom -> failwith "called Translate.procEntryExit on Bottom"
    | Level (_parent, frame, _id) ->
        let body = unNx body in
        let body = Frame.procEntryExit1 (frame, body) in
        frags := Frame.Proc { body; frame } :: !frags;
        ()

  let get_result () = !frags
end
