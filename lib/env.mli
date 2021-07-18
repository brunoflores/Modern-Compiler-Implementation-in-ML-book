(** We have two environments: a type environment and a value environment.
    The following program demonstrates that one environment will not suffice:

    {v
  let type a = int
      var a : a := 5
      var b : a := a
  in
      b + a
  end
    v}

    The symbol a denotes the type "a" in syntactic contexts where type
    identifiers are expected, and the variable "a" in syntactic contexts
    where variables are expected. *)

type ty_entry = Types.ty
(** For a type identifier, we need to remember only the type that it
    stands for. *)

(** For a value identifier, we need to know whether it is a variable or
    a function. If a variable, what is its type. If a function,
    what are its parameter and result types, and so on. *)
type env_entry =
  | VarEntry of { ty : ty_entry }
  | FunEntry of {
      formals : ty_entry list;
      result : ty_entry;
          (** The result type can be {!Types.ty.UNIT} for procedures. *)
    }

val base_tenv : ty_entry Symbol.table
(** Predefined types. *)

val base_venv : env_entry Symbol.table
(** Predefined functions. *)
