(** Bindings for the compiler.
    The primitive types are int and string.
    All types are either primitive types or constructed using records
    and arrays from other (primitive, record, or array) types. *)

type unique = unit ref
(** Every "record type expression" creates a new and different record type,
    even if the fields are similar. *)

(** The following is illegal in Tiger because structurally equivalent types
    are NOT interchangeable:

    {v
  let type a = {x: int, y: int}
      type b = {x: int, y: int}
      var i : a := ...
      var j : b := ...
  in
      i := j
  end
    v}

    However, this is legal because c is the same type as a:

    {v
  let type a = {x: int, y: int}
      type c = a
      var i : a := ...
      var j : c := ...
  in
      i := j
  end
    v}

    It is NOT the type declaration that causes a new and distinct type to be
    made, but the type expression [{x: int, y: int}]. *)

type ty =
  | Int  (** Primitive *)
  | String  (** Primitive *)
  | Nil  (** Belongs to any record type *)
  | Unit  (** The type with just one value *)
  | Record of (Symbol.symbol * ty) list * unique
  | Array of ty * unique
  | Name of Symbol.symbol * ty option ref

(** The type [Unit] is for expressions that return "no value". *)

(** The [Record] constructor carries the names and types of the fields,
    and a "unique" value to distinguish this record type from others. *)

(** The [Array] constructor carries the type of the array elements and a
    "unique" value to distinguish this array type from others. *)

(** The [Name] constructor is for processing mutually recursive types,
    when we need a place-holder for types whose name we know but whose
    definition we have not yet seen.
    The type [Name (sym, ref (Some t))] is equivalent to type [t];
    But [Name (sym, ref None)] is just the place-holder. *)
