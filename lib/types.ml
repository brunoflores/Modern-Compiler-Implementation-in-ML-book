(* Bindings for the compiler.
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
  | INT  (** Primitive. *)
  | STRING  (** Primitive. *)
  | NIL  (** Belongs to any record type. *)
  | UNIT  (** "no value". *)
  | RECORD of (Symbol.symbol * ty) list * unique
  | ARRAY of ty * unique
  | NAME of Symbol.symbol * ty option ref

(** The type [UNIT] is for expressions that return "no value". *)

(** The [RECORD] constructor carries the names and types of the fields,
    and a "unique" value to distinguish this record type from others. *)

(** The [ARRAY] constructor carries the type of the array elements and a
    "unique" value to distinguish this array type from others. *)

(** The [NAME] constructor is for processing mutually recursive types,
    when we need a place-holder for types whose name we know but whose
    definition we have not yet seen.
    The type [NAME (sym, ref (SOME t))] is equivalent to type [t];
    But [NAME (sym, ref NONE)] is just the place-holder. *)
