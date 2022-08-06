module type S = sig
  (** Type-checking. *)

  val transProg : Tiger.exp -> (unit, (Tiger.pos option * string) list) result
  (** Type-checks a given program and returns unit on success or a list of all
    errors identified. *)
end

module Make
    (Env : Env.S)
    (Translate : Translate.I
                   with type access = Env.access
                    and type level = Env.level) : S
