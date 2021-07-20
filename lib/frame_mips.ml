module Frame_mips : Frame.I = struct
  type frame = unit
  (** [frame] is a data structure that holds:
      - Location of all the formals,
      - Instructions required to implement the "view shift",
      - Number of locals allocated so far,
      - The [label] at which the function's machine code is to begin. *)

  (* type access = InFrame of int | InReg of Temp.temp *)
  type access = unit

  (** [InFrame (x)] indicates a memory location at offset [x] from the
      frame pointer.
      [InReg r1] indicates that it will be held in "register" [r1]. *)

  let new_frame (_ : Temp.label) (_ : bool list) : frame =
    failwith "not implemented"

  let name _ : Temp.label = failwith "not implemented"

  let formals _ : access list = failwith "not implemented"

  let alloc_local _ _ : access = failwith "not implemented"
end
