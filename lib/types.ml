(* Bindings for the compiler.
   The primitive types are int and string.
   All types are either primitive types or constructed using records
   and arrays from other (primitive, record, or array) types. *)

(* Every "record type expression" creates a new and different record type,
   even if the fields are similar. *)
type unique = unit ref

(* The following is illegal in Tiger because structurally equivalent types
   are NOT interchangeable:

   let type a = {x: int, y: int}
       type b = {x: int, y: int}
       var i : a := ...
       var j : b := ...
   in
       i := j
   end

   However, this is legal because c is the same type as a:

   let type a = {x: int, y: int}
       type c = a
       var i : a := ...
       var j : c := ...
   in
       i := j
   end

   It is NOT the type declaration that causes a new and distinct type to be
   made, but the type expression {x: int, y: int}. *)

type ty =
  (* Primitive. *)
  | INT
  (* Primitive. *)
  | STRING
  (* The expression nil belongs to any record type. *)
  | NIL
  (* The type unit is for expressions that return "no value". *)
  | UNIT
  (* The RECORD constructor carries the names and types of the fields,
     and a "unique" value to distinguish this record type from others. *)
  | RECORD of (Symbol.symbol * ty) list * unique
  (* The ARRAY constructor carries the type of the array elements and a
     "unique" value to distinguish this array type from others. *)
  | ARRAY of ty * unique
  (* The NAME constructor is for processing mutually recursive types,
     when we need a place-holder for types whose name we know but whose
     definition we have not yet seen.
     The type
        NAME (sym, ref (SOME t))
     is equivalent to type t;
     But
        NAME (sym, ref NONE)
     is just the place-holder. *)
  | NAME of Symbol.symbol * ty option ref
