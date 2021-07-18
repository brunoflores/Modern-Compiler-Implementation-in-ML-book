(* Type-checks a given program and returns unit on success or a list of all
   errors identified. *)
val transProg : Tiger.exp -> (unit, (Tiger.pos option * string) list) result
