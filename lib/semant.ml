open Core

type venv = Env.env_entry Symbol.table

type tenv = Types.ty Symbol.table

type expty = { exp : Translate.exp; ty : Types.ty }

let rec transExp (venv : venv) (tenv : tenv) (exp : Tiger.exp) :
    (expty, (Tiger.pos option * string) list) result =
  let rec checkInt (e : (expty, (Tiger.pos option * string) list) result) =
    let check e =
      let _, pos = e.exp in
      match e.ty with Types.INT -> Ok e | _ -> Error (pos, "integer expected")
    in
    match e with
    | Ok e -> (
        match check e with Ok _ as e -> e | Error err -> Error [ err ])
    | Error _ as errs -> errs
  (* let rec checkIntBinOp errs left right =
       let leftty, lefterrs = trexp errs left in
       let rightty, righterrs = trexp errs right in
       match (checkInt leftty, checkInt rightty) with
       | Ok _, Ok _ -> lefterrs @ righterrs
       | Error err', Error err'' -> err' :: err'' :: lefterrs @ righterrs
       | _, Error err' | Error err', _ -> err' :: lefterrs @ righterrs
     and checkBinOp errs left right pos =
       let leftty, lefterrs = trexp errs left in
       let rightty, righterrs = trexp errs right in
       match (leftty, rightty) with
       | { ty = Types.STRING }, { ty = Types.STRING } -> Ok Types.STRING
       | { ty = Types.INT }, { ty = Types.INT } -> Ok Types.INT
       | { ty = Types.INT }, _
       | { ty = Types.STRING }, _
       | { ty = Types.NIL }, _
       | { ty = Types.UNIT }, _
       | { ty = Types.RECORD (_, _) }, _
       | { ty = Types.ARRAY (_, _) }, _
       | { ty = Types.NAME (_, _) }, _ ->
           Error ((Some pos, "both sides must be string or int") :: errs) *)
  and trexp (errs : (Tiger.pos option * string) list) (e : Tiger.exp) :
      (expty, (Tiger.pos option * string) list) result =
    match e with
    | Tiger.NilExp -> Ok { exp = ((), None); ty = Types.NIL }
    | Tiger.StringExp (_, pos) -> Ok { exp = ((), Some pos); ty = Types.STRING }
    | Tiger.IntExp _ -> Ok { exp = ((), None); ty = Types.INT }
    | Tiger.SeqExp ((e, _) :: _) -> trexp errs e
    | Tiger.VarExp (Tiger.SimpleVar (x, pos)) -> (
        match Symbol.look x venv with
        | Some (Env.VarEntry { ty }) -> Ok { exp = ((), Some pos); ty }
        | Some (FunEntry _) -> failwith "not implemented"
        | None ->
            Error
              (( Some pos,
                 Format.asprintf "variable %S undefined" (Symbol.name x) )
               :: errs))
    | Tiger.LetExp { decs; body; _ } ->
        let venv, tenv = trdec errs decs in
        transExp venv tenv body
    | Tiger.OpExp { left; oper = Tiger.PlusOp; right; pos } -> (
        let lefty = checkInt (trexp errs left) in
        let righty = checkInt (trexp errs right) in
        match (lefty, righty) with
        | Ok _, Ok _ -> Ok { exp = ((), Some pos); ty = Types.INT }
        | (Error _ as l), Ok _ -> l
        | Ok _, (Error _ as r) -> r
        | Error l, Error r -> Error (l @ r))
    | _ -> Error errs
  and trdec (errs : (Tiger.pos option * string) list) (decs : Tiger.dec list) =
    let venv =
      List.map decs ~f:(function
        | Tiger.VarDec { name; init; _ } -> (
            match trexp errs init with
            | Ok { ty; _ } -> (Env.VarEntry { ty }, name)
            | Error _ -> failwith "FATAL")
        | Tiger.FunctionDec _ | Tiger.TypeDec _ -> failwith "not implemented")
      |> List.fold ~init:venv ~f:(fun acc (x, name) -> Symbol.enter name x acc)
    in
    (venv, tenv)
    (*
     | Tiger.OpExp { left; oper = Tiger.DivideOp; right; pos } ->
         ({ exp = ((), None); ty = Types.INT }, checkIntBinOp errs left right)
     | Tiger.OpExp { left; oper = Tiger.TimesOp; right; pos } ->
         ({ exp = ((), None); ty = Types.INT }, checkIntBinOp errs left right)
     | Tiger.OpExp { left; oper = Tiger.MinusOp; right; pos } ->
         ({ exp = ((), None); ty = Types.INT }, checkIntBinOp errs left right)
     | Tiger.OpExp { left; oper = Tiger.GeOp; right; pos } -> (
         match checkBinOp errs left right pos with
         | Ok Types.STRING -> ({ exp = ((), Some pos); ty = Types.STRING }, errs)
         | Ok Types.INT -> ({ exp = ((), Some pos); ty = Types.INT }, errs)
         | Ok _ -> failwith "cannot happen"
         | Error errs -> ({ exp = ((), Some pos); ty = Types.INT }, errs))
     | Tiger.OpExp { left; oper = Tiger.GtOp; right; pos } -> (
         match checkBinOp errs left right pos with
         | Ok Types.STRING -> ({ exp = ((), Some pos); ty = Types.STRING }, errs)
         | Ok Types.INT -> ({ exp = ((), Some pos); ty = Types.INT }, errs)
         | Ok _ -> failwith "cannot happen"
         | Error errs -> ({ exp = ((), Some pos); ty = Types.INT }, errs))
     | Tiger.OpExp { left; oper = Tiger.LeOp; right; pos } -> (
         match checkBinOp errs left right pos with
         | Ok Types.STRING -> ({ exp = ((), Some pos); ty = Types.STRING }, errs)
         | Ok Types.INT -> ({ exp = ((), Some pos); ty = Types.INT }, errs)
         | Ok _ -> failwith "cannot happen"
         | Error errs -> ({ exp = ((), Some pos); ty = Types.INT }, errs))
     | Tiger.OpExp { left; oper = Tiger.LtOp; right; pos } -> (
         match checkBinOp errs left right pos with
         | Ok Types.STRING -> ({ exp = ((), Some pos); ty = Types.STRING }, errs)
         | Ok Types.INT -> ({ exp = ((), Some pos); ty = Types.INT }, errs)
         | Ok _ -> failwith "cannot happen"
         | Error errs -> ({ exp = ((), Some pos); ty = Types.INT }, errs))
     | Tiger.OpExp { left; oper = Tiger.NeqOp; right; pos } -> (
         match checkBinOp errs left right pos with
         | Ok Types.STRING -> ({ exp = ((), Some pos); ty = Types.STRING }, errs)
         | Ok Types.INT -> ({ exp = ((), Some pos); ty = Types.INT }, errs)
         | Ok _ -> failwith "cannot happen"
         | Error errs -> ({ exp = ((), Some pos); ty = Types.INT }, errs))
     | Tiger.OpExp { left; oper = Tiger.EqOp; right; pos } -> (
         match checkBinOp errs left right pos with
         | Ok Types.STRING -> ({ exp = ((), Some pos); ty = Types.STRING }, errs)
         | Ok Types.INT -> ({ exp = ((), Some pos); ty = Types.INT }, errs)
         | Ok _ -> failwith "cannot happen"
         | Error errs -> ({ exp = ((), Some pos); ty = Types.INT }, errs))
     | Tiger.VarExp var ->
         let expty, varerrs = trvar errs var in
         (expty, varerrs @ errs) *)

    (* and trvar errs = function
       | Tiger.SimpleVar (id, pos) -> (
           match Symbol.look id venv with
           | Some (Env.VarEntry _) ->
               ({ exp = ((), Some pos); ty = Types.NAME (id, ref None) }, errs)
           | Some (FunEntry _) | None ->
               ( { exp = ((), Some pos); ty = Types.INT },
                 (Some pos, "undefined variable") :: errs ))
       | Tiger.FieldVar (obj, field, _) -> (
           match obj with
           | Tiger.SimpleVar (id, pos) -> (
               match Symbol.look id venv with
               | Some (Env.VarEntry { ty = Types.RECORD (fields, _) }) -> (
                   match List.find fields (fun (x, _) -> phys_equal x field) with
                   | Some _ -> ({ exp = ((), Some pos); ty = Types.INT }, errs)
                   | None ->
                       ( { exp = ((), Some pos); ty = Types.INT },
                         (Some pos, "field is not a member of the record") :: errs
                       ))
               | None ->
                   ( { exp = ((), Some pos); ty = Types.INT },
                     (Some pos, "undefined record") :: errs )
               | _ -> ({ exp = ((), None); ty = Types.INT }, errs))
           | _ -> ({ exp = ((), None); ty = Types.INT }, errs))
       | Tiger.SubscriptVar (_, _, _) ->
           ({ exp = ((), None); ty = Types.INT }, errs) *)
  in

  trexp [] exp

let transProg (exp : Tiger.exp) =
  let transWithBase exp = transExp Env.base_venv Env.base_tenv exp in
  match transWithBase exp with Ok _ -> Ok () | Error _ as err -> err
