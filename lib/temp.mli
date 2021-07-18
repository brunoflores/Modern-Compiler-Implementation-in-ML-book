type temp

val new_temp : unit -> temp

include Table.I

val make_string : temp -> string

type label = Symbol.symbol

val new_label : unit -> label

val named_label : string -> label
