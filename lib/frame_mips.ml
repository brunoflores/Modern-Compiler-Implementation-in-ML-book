module Make : Frame.S = struct
  type frame = unit
  (* [frame] is a data structure that holds:
     - Location of all the formals,
     - Instructions required to implement the "view shift",
     - Number of locals allocated so far,
     - The [label] at which the function's machine code is to begin (page 140).
  *)

  type access = InFrame of int | InReg of Temp.temp
  (* [InFrame x] indicates a memory location at offset [x] from the
     frame pointer. [InReg r1] indicates that it will be held in
     "register" [r1].
  *)

  (* "shift of view", page 136: For each formal parameter, [new_frame]
      must calculate:
      - How the parameter will be seen from inside the function
        (in a register, or in a frame location);
      - What instructions must be produced to implement the "view shift".

      Example: A frame-resident parameter will be seen as "memory at offset
      X from the frame pointer", and the view shift will be implemented
      by copying the stack pointer to the frame pointer on entry to the
      procedure.
  *)
  let new_frame (_ : Temp.label) (_ : bool list) : frame =
    failwith "not implemented"

  let name _ : Temp.label = failwith "not implemented"
  let formals _ : access list = failwith "not implemented"
  let alloc_local _ _ : access = failwith "not implemented"
end
