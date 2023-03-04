(** An implementation of {!Frame.S} that is machine-dependent on the LC3. *)

module Make : Frame.S = struct
  type access =
    | InFrame of int
        (** [InFrame x] indicates a memory location at offset [x] from the
            frame pointer. *)
  (* | InReg of Temp.temp *)
  (*     (\** [InReg r1] indicates that it will be held in "register" [r1]. *\) *)

  type frame = { label : Temp.label; locals : int; formals : access list }
  (** [frame] is a data structure that holds:
      - Location of all the formals,
      - Instructions required to implement the "view shift",
      - Number of locals allocated so far,
      - The [label] at which the function's machine code is to begin
        (page 140). *)

  type framestbl = frame Symbol.table ref

  let frames : framestbl = ref Symbol.empty
  let offset i = i * 4 * -1

  (* "shift of view", page 136: For each formal parameter, [new_frame]
      must calculate:
      - How the parameter will be seen from inside the function
        (in a register, or in a frame location);
      - What instructions must be produced to implement the "view shift".

      Example: A frame-resident parameter will be seen as "memory at offset
      X from the frame pointer", and the view shift will be implemented
      by copying the stack pointer to the frame pointer on entry to the
      procedure. *)
  let new_frame (label : Temp.label) (escape : bool list) : frame =
    let formals =
      List.mapi
        (fun i escape ->
          if escape then InFrame (offset i)
          else failwith "not implemented: new_frame with escape = false")
        escape
    in
    let frame = { label; locals = 0; formals } in
    frames := Symbol.enter (!frames, label, frame);
    frame

  let name { label; _ } : Temp.label = label
  let formals { formals; _ } : access list = formals

  let alloc_local ({ locals; label; _ } as frame) escape : access =
    if escape then (
      let locals' = locals + 1 in
      (* Really "overwrite" not "enter". *)
      frames := Symbol.enter (!frames, label, { frame with locals = locals' });
      InFrame (offset locals'))
    else failwith "not implemented: alloc_local with escape = false"
end
