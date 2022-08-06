(** [Semant] performs semantic analysis - including type-checking -
    of abstract syntax. *)

module type S = sig
  val trans_prog : Tiger.exp -> (unit, (Tiger.pos option * string) list) result
  (** Type-checks a given program and returns unit on success or a list of all
    errors identified. *)
end

(** Functor interface. *)
module Make
    (Env : Env.S)
    (Translate : Translate.I
                   with type access = Env.access
                    and type level = Env.level) : S
