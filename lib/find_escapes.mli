(** Calculate escapes. *)

val findEscape : Tiger.exp -> unit
(** Traverse the entire AST looking for escaping variables.
    Record this information in the AST using side-effects. *)
