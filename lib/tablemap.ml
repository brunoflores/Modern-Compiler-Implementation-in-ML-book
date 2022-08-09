module Make (Key : Map.OrderedType) : Table.S with type key = Key.t = struct
  module M = Map.Make (Key)

  type key = Key.t
  type 'a table = 'a M.t

  let empty = M.empty
  let enter (table, (symbol : key), t) : 'a table = M.add symbol t table

  let look ((table : 'a table), (symbol : key)) : 'a option =
    try Some (M.find symbol table) with Not_found -> None
end
