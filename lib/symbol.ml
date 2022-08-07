(* This representation of symbol is to support the [name] function. *)
type symbol = string * int [@@deriving show]

let equal ((_, s1) : symbol) ((_, s2) : symbol) = s1 = s2

(* The next available symbol identifier.  *)
let next = ref 0

(* Use a hash table with destructive update.
   We'll never need previous versions of [string -> symbol]. *)
let hash_table : (string, int) Hashtbl.t = Hashtbl.create 10 (* FIXME *)

let create (name : string) : symbol =
  let i =
    match Hashtbl.find_opt hash_table name with
    | Some i -> i
    | None ->
        let i = !next in
        next := i + 1;
        Hashtbl.add hash_table name i;
        i
  in
  (name, i)

let name ((s, _) : symbol) : string = s

(* -------------------------------------------------------------------------- *)

(* Use our map implementation to keep a potentially large number of
   environments around. *)
module Table = Tablemap.Make (struct
  type t = symbol

  let compare (_, i0) (_, i1) = Int.compare i0 i1
end)

type 'a table = 'a Table.table

let empty = Table.empty
let enter = Table.enter
let look = Table.look
