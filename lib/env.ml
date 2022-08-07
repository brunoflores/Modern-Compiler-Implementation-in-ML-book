module type S = sig
  type ty_entry = Types.ty
  type access
  type level

  type env_entry =
    | VarEntry of { (* access : access; *) ty : ty_entry }
    | FunEntry of { formals : ty_entry list; result : ty_entry; level : level }

  val base_tenv : ty_entry Symbol.table
  val base_venv : env_entry Symbol.table
end

module Make (Translate : Translate.S) :
  S with type level = Translate.level and type access = Translate.access =
struct
  (* Base type environment: *)
  type ty_entry = Types.ty

  let base_tenv : ty_entry Symbol.table =
    (* This notation can be improved with partial application and the |>
       operator, but that requires changes in the Symbol interface. *)
    let tenv = Symbol.empty in
    let tenv = Symbol.enter (tenv, Symbol.create "int", Types.Int) in
    let tenv = Symbol.enter (tenv, Symbol.create "string", Types.String) in
    tenv

  (* Base value environment: *)

  type access = Translate.access
  type level = Translate.level

  type env_entry =
    | VarEntry of { (* access : access; *) ty : ty_entry }
    | FunEntry of { formals : ty_entry list; result : ty_entry; level : level }

  let base_venv : env_entry Symbol.table = Symbol.empty

  (* Appendix A: *)

  (* let base_venv : env_entry Symbol.table =
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
          (FunEntry { formals = [ Types.INT ]; result = Types.UNIT }) *)
end
