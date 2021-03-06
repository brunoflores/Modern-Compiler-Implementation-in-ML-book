(** Temporaries and labels.

    We use the word "temporary" to mean a value that is temporarily held in
    a register, and the word "label" to mean some machine-language location
    whose exact address is yet to be determined.

    This module manages these two distinct sets of names. *)

type temp
(** Abstract name for a local variable. *)

val new_temp : unit -> temp
(** Returns a new temporary from an infinite set of temps. *)

val make_string : temp -> string

type label = Symbol.symbol
(** Abstract name for a static memory address. *)

val new_label : unit -> label
(** Returns a new label from an infinite set of labels. *)

val named_label : string -> label
(** Returns a new label whose assembly-language name is {i string}. *)

include Table.I
