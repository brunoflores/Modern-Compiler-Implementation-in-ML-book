type symbol = string * int

let next = ref 0
let hash_table : (string, int) Hashtbl.t = Hashtbl.create 10

let create name : symbol =
  let i =
    try Hashtbl.find hash_table name
    with Not_found ->
      let i = !next in
      next := i + 1;
      Hashtbl.add hash_table name i;
      i
  in
  (name, i)

let name ((s, _) : symbol) = s

module Table = Table_map.TableMap (struct
  type t = symbol

  let compare (name0, i0) (name1, i1) =
    match String.compare name0 name1 with 0 -> Int.compare i0 i1 | c -> c
end)

type 'a table = 'a Table.table

let empty = Table.empty
let enter = Table.enter
let look = Table.look
