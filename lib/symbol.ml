open Core

module Symbol = struct
  module T = struct
    type t = string * int [@@deriving compare, sexp, show]
  end

  include T
  include Comparable.Make (T)
end

type symbol = Symbol.t [@@deriving compare, show]

let next = ref 0

let hash_table = Hashtbl.create (module String)

let create name : symbol =
  match Hashtbl.find hash_table name with
  | Some n -> (name, n)
  | None ->
      let i = !next in
      next := i + 1;
      Hashtbl.set hash_table ~key:name ~data:i;
      (name, i)

let name ((s, _) : symbol) = s

module Table = Table_map.TableMap (Symbol)

type 'a table = 'a Table.table

let empty = Table.empty

let enter = Table.enter

let look = Table.look
