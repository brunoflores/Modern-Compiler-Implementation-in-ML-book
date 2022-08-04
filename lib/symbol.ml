module Symbol = struct
  module T = struct
    type t = string * int [@@deriving compare, sexp, show]
  end

  include T
  include Comparable.Make (T)
end

type symbol = Symbol.t [@@deriving compare, show]

let next = ref 0

module Hash_table = Hashtbl.Make (String)

let hash_table = Hash_table.create 10

let create name : symbol =
  match Hash_table.find hash_table name with
  | Some n -> (name, n)
  | None ->
      let i = !next in
      next := i + 1;
      Hash_table.add hash_table name i;
      (name, i)

let name ((s, _) : symbol) = s

module Table = Table_map.TableMap (Symbol)

type 'a table = 'a Table.table

let empty = Table.empty
let enter = Table.enter
let look = Table.look
