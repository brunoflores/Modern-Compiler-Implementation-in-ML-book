(* Base type environment: *)
type ty_entry = Types.ty

let base_tenv : ty_entry Symbol.table =
  Symbol.empty
  |> Symbol.enter (Symbol.create "int") Types.INT
  |> Symbol.enter (Symbol.create "string") Types.STRING

(* Base value environment: *)

type env_entry =
  | VarEntry of { ty : ty_entry }
  | FunEntry of { formals : ty_entry list; result : ty_entry }

let base_venv : env_entry Symbol.table =
  Symbol.empty
  |> Symbol.enter (Symbol.create "print")
       (FunEntry { formals = [ Types.STRING ]; result = Types.NIL })
  |> Symbol.enter (Symbol.create "flush")
       (FunEntry { formals = []; result = Types.UNIT })
  |> Symbol.enter (Symbol.create "getchar")
       (FunEntry { formals = []; result = Types.STRING })
  |> Symbol.enter (Symbol.create "ord")
       (FunEntry { formals = [ Types.STRING ]; result = Types.INT })
  |> Symbol.enter (Symbol.create "chr")
       (FunEntry { formals = [ Types.INT ]; result = Types.STRING })
  |> Symbol.enter (Symbol.create "size")
       (FunEntry { formals = [ Types.STRING ]; result = Types.INT })
  |> Symbol.enter
       (Symbol.create "substring")
       (FunEntry
          {
            formals = [ Types.STRING; Types.INT; Types.INT ];
            result = Types.STRING;
          })
  |> Symbol.enter (Symbol.create "concat")
       (FunEntry
          { formals = [ Types.STRING; Types.STRING ]; result = Types.STRING })
  |> Symbol.enter (Symbol.create "not")
       (FunEntry { formals = [ Types.INT ]; result = Types.INT })
  |> Symbol.enter (Symbol.create "exit")
       (FunEntry { formals = [ Types.INT ]; result = Types.UNIT })
