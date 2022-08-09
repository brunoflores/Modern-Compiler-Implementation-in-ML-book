module Temp = struct
  type t = int

  let compare = Int.compare
end

type temp = Temp.t

let compare = Temp.compare
let new_temp (_ : unit) : temp = failwith "not implemented"
let make_string (_ : temp) : string = failwith "not implemented"

type label = Symbol.symbol

let new_label (_ : unit) : label = failwith "not implemented"
let named_label (_ : string) : label = failwith "not implemented"

module Table = Tablemap.Make (Temp)

type key = temp
type 'a table = 'a Table.table

let empty = Table.empty
let enter = Table.enter
let look = Table.look
