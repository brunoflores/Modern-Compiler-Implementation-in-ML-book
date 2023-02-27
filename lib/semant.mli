(** [Semant] performs semantic analysis - including type-checking -
    of abstract syntax. *)

module type S = sig
  val trans_prog : Tiger.exp -> unit
  (** Type-checks a given program and returns unit on success. *)
end

(** Functor interface. *)
module Make
    (Env : Env.S)
    (Translate : Translate.S
                   with type access = Env.access
                    and type level = Env.level) : S
