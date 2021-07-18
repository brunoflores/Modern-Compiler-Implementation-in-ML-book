open Core

module TableMap (Key : Comparable) : Table.I with type key = Key.t = struct
  type key = Key.t

  type 'a table = (key, 'a, Key.comparator_witness) Map.t

  let empty = Map.empty (module Key)

  let enter (symbol : key) t table : 'a table =
    Map.set table ~key:symbol ~data:t

  let look (symbol : key) (table : 'a table) : 'a option = Map.find table symbol
end
