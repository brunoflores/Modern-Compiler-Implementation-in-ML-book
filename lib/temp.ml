module Temp = struct
  module T = struct
    (* type t = int [@@deriving compare, sexp, show] *)
    type t = int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type temp = Temp.t
type key = temp

let new_temp (_ : unit) : temp = failwith "not implemented"

module Table = Table_map.TableMap (Temp)

type 'a table = 'a Table.table

let empty = Table.empty
let enter = Table.enter
let look = Table.look
let make_string (_ : temp) : string = failwith "not implemented"

type label = Symbol.symbol

let new_label (_ : unit) : label = failwith "not implemented"
let named_label (_ : string) : label = failwith "not implemented"
