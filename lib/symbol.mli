(** Abstract representation of symbol tables (environments).
    It provides fast lookup and is intented for cases where lookups occur
    more frequently than inserts. *)

type symbol [@@deriving show]
(** Abstract symbol. [symbol] provides efficient lookup of names in
    environments by avoiding string comparisons. *)

val equal : symbol -> symbol -> bool

val create : string -> symbol
(** Transform a string into a symbol. *)

val name : symbol -> string
(** Recover the name of a symbol. *)

type 'a table
(** Symbol table. Environments are tables mapping [symbols] to bindings.
    That is, an ['a table] is a mapping from [symbol] to ['a], whether ['a]
    is a type binding, or a value binding, or any other kind of binding. *)

val empty : 'a table
(** Empty symbol table. *)

val enter : 'a table * symbol * 'a -> 'a table
(** Enter a symbol in the table. *)

val look : 'a table * symbol -> 'a option
(** Look up a symbol in the table. *)
