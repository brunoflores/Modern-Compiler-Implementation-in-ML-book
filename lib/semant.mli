module type I = sig
  (** Type-checking. *)

  val transProg : Tiger.exp -> (unit, (Tiger.pos option * string) list) result
  (** Type-checks a given program and returns unit on success or a list of all
    errors identified. *)
end
