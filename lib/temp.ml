(* module Temp = struct *)
(*   type t = int [@@deriving show] *)

(*   let compare = Int.compare *)
(* end *)

(* module TempTable = Tablemap.Make (Temp) *)

(* type 'a table = 'a TempTable.table *)

(* let empty = TempTable.empty *)
(* let enter = TempTable.enter *)
(* let look = TempTable.look *)

(* type temp = Temp.t [@@deriving show] *)

type temp = int [@@deriving show]

let current_temp : temp ref = ref 0

let new_temp _ : temp =
  let temp = !current_temp in
  incr current_temp;
  temp

let make_string temp : string = Format.sprintf "r%d" temp

type label = Symbol.symbol [@@deriving show]

let current_label : int ref = ref 0

let new_label _ : label =
  let label = !current_label in
  incr current_label;
  Symbol.create @@ Format.sprintf "l%d" label

let named_label s : label = Symbol.create s
