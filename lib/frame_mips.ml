(* module FrameMips : Frame.I = struct
  type frame = unit
  (** [frame] is a data structure that holds:
      - Location of all the formals,
      - Instructions required to implement the "view shift",
      - Number of locals allocated so far,
      - The [label] at which the function's machine code is to begin. *)

  (** [InFrame (x)] indicates a memory location at offset [x] from the
      frame pointer.
      [InReg r1] indicates that it will be held in "register" [r1]. *)
  type access = InFrame of int | InReg of Temp.temp

  let new_frame (name : Temp.label) (formals : bool list) : frame =
    failwith "not implemented"

  let name frame : Temp.label = failwith "not implemented"

  let formals frame : access list = failwith "not implemented"

  let alloc_local frame bool : access = failwith "not implemented"
end *)
