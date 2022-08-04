module TableMap (Key : Map.OrderedType) : Table.I with type key = Key.t = struct
  module M = Map.Make (Key)

  type key = Key.t
  type 'a table = 'a M.t

  let empty = M.empty
  let enter (symbol : key) t table : 'a table = M.add symbol t table

  let look (symbol : key) (table : 'a table) : 'a option =
    try Some (M.find symbol table) with Not_found -> None
end
