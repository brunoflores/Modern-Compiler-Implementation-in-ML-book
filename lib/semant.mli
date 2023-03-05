(** [Semant] performs semantic analysis - including type-checking -
    of abstract syntax. *)

module type S = sig
  val trans_prog : Tiger.exp -> (unit, (Tiger.pos option * string) list) result
  (** Type-checks a program while translating it to [Tree]. *)
end

(** Functor interface. *)
module Make
    (Env : Env.S)
    (Translate : Translate.S
                   with type access = Env.access
                    and type level = Env.level) : S
