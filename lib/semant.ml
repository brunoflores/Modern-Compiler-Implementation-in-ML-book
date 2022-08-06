module type S = sig
  val trans_prog : Tiger.exp -> (unit, Tiger.pos option * string) result
end

module Make
    (Env : Env.S)
    (Translate : Translate.S
                   with type access = Env.access
                    and type level = Env.level) : S = struct
  type venv = Env.env_entry Symbol.table
  type tenv = Types.ty Symbol.table

  (* The result is [expty], containing a translated expression and its
     Tiger-language type. *)
  type expty = { exp : Translate.exp; ty : Types.ty }
  type error = Tiger.pos option * string
  type binop_ty = String | Int

  (* Augment a given environment with declarations. *)
  let rec trans_decs (venv : venv) (tenv : tenv) (decs : Tiger.dec list) :
      venv * tenv =
    let venv' =
      List.map
        (function
          | Tiger.VarDec { name; init; typ = None; _ } -> (
              match trans_exp venv tenv init with
              | Ok { ty; _ } ->
                  (Env.VarEntry { access = Translate.alloc_local (); ty }, name)
              | Error _ -> failwith "")
          | Tiger.VarDec _ -> failwith "not implemented"
          | Tiger.FunctionDec _ -> failwith "not implemented"
          | Tiger.TypeDec _ -> failwith "not implemented")
        decs
      |> List.fold_left
           (fun acc (env_entry, sym) -> Symbol.enter (acc, sym, env_entry))
           venv
    in
    (venv', tenv)

  and _trans_ty (_tenv : tenv) (_ty : Tiger.ty) : Types.ty =
    failwith "not implemented"

  and _trans_dec (_venv : venv) (_tenv : tenv) (_dec : Tiger.dec) : venv * tenv
      =
    failwith "not implemented"

  and _trans_var (_venv : venv) (_tenv : tenv) (_var : Tiger.var) : expty =
    failwith "not implemented"

  and trans_exp (venv : venv) (tenv : tenv) (exp : Tiger.exp) :
      (expty, error) result =
    let rec check_int (e : (expty, error) result) =
      match e with
      | Ok expty -> (
          match expty.ty with
          | Types.Int -> e
          | _ -> Error (None, "integer required"))
      | Error _ -> e
    and check_int_binop left right : (unit, error) result =
      let lefty = check_int (trexp left) in
      let righty = check_int (trexp right) in
      match (lefty, righty) with
      | Ok _, Ok _ -> Ok ()
      | (Error _ as err), _ | _, (Error _ as err) -> err
    and check_binop left right pos : (binop_ty, error) result =
      let lefty = trexp left in
      let righty = trexp right in
      match (lefty, righty) with
      | Ok { ty = Types.String; _ }, Ok { ty = Types.String; _ } -> Ok String
      | Ok { ty = Types.Int; _ }, Ok { ty = Types.Int; _ } -> Ok Int
      | _ -> Error (Some pos, "both sides must be string or int")
    and actual_seq_ty (exps : (Tiger.exp * Tiger.pos) list) :
        (expty, error) result =
      match exps with
      | [] -> Ok { exp = ((), None); ty = Types.Unit }
      | [ (exp, _pos) ] -> trexp exp
      | (exp, pos) :: exps' -> (
          match trexp exp with
          | Ok { ty = Types.Unit; _ } -> actual_seq_ty exps'
          | Ok { ty = _; _ } ->
              Error
                ( Some pos,
                  "this expression is on the left side of a sequence: expected \
                   unit" )
          | Error _ as err -> err)
    and trexp (e : Tiger.exp) : (expty, error) result =
      match e with
      | Tiger.NilExp -> Ok { exp = ((), None); ty = Types.Nil }
      | Tiger.StringExp (_, pos) ->
          Ok { exp = ((), Some pos); ty = Types.String }
      | Tiger.IntExp _ -> Ok { exp = ((), None); ty = Types.Int }
      | Tiger.SeqExp exps -> actual_seq_ty exps
      | Tiger.VarExp var -> trvar var
      | Tiger.LetExp { decs; body; _ } ->
          let venv', tenv' = trans_decs venv tenv decs in
          trans_exp venv' tenv' body
      | Tiger.OpExp { left; oper = Tiger.PlusOp; right; _ }
      | Tiger.OpExp { left; oper = Tiger.DivideOp; right; _ }
      | Tiger.OpExp { left; oper = Tiger.TimesOp; right; _ }
      | Tiger.OpExp { left; oper = Tiger.MinusOp; right; _ } -> (
          match check_int_binop left right with
          | Ok _ -> Ok { exp = ((), None); ty = Types.Int }
          | Error _ as err -> err)
      | Tiger.OpExp { left; oper = Tiger.GeOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.GtOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.LeOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.LtOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.NeqOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.EqOp; right; pos } -> (
          match check_binop left right pos with
          | Ok String -> Ok { exp = ((), Some pos); ty = Types.String }
          | Ok Int -> Ok { exp = ((), Some pos); ty = Types.Int }
          | Error _ as err -> err)
      | Tiger.CallExp _ | Tiger.RecordExp _ | Tiger.AssignExp _ | Tiger.IfExp _
      | Tiger.WhileExp _ | Tiger.ForExp _ | Tiger.BreakExp _ | Tiger.ArrayExp _
        ->
          failwith ""
    and trvar (var : Tiger.var) : (expty, error) result =
      let rec actual_ty = function
        | ( Types.Int | Types.String | Types.Nil | Types.Unit | Types.Record _
          | Types.Array _ ) as t ->
            Ok t
        | Types.Name (id, ty_opt) -> (
            let ty = !ty_opt in
            match ty with
            | Some ty -> actual_ty ty
            | None ->
                Error (Format.sprintf "undefined type: %s" (Symbol.name id)))
      in
      let rec walk_record (var : Tiger.var) (field : Symbol.symbol) :
          (expty, error) result =
        match var with
        | Tiger.SimpleVar (id, pos) -> (
            match Symbol.look (venv, id) with
            | Some (Env.VarEntry { ty = Types.Record (fields, _); _ }) -> (
                match List.find_opt (fun (x, _) -> x = field) fields with
                | Some (_, ty) -> Ok { exp = ((), None); ty }
                | None -> Error (Some pos, "field is not a member of the record")
                )
            | Some _ -> Error (Some pos, "var is not a record")
            | None -> Error (Some pos, "undefined record"))
        | Tiger.FieldVar (var, field, _) -> walk_record var field
        | Tiger.SubscriptVar _ -> failwith "not implemented"
      in
      match var with
      | Tiger.SimpleVar (id, pos) -> (
          match Symbol.look (venv, id) with
          | Some (Env.VarEntry { ty; _ }) -> (
              let ty_res = actual_ty ty in
              match ty_res with
              | Ok ty -> Ok { exp = ((), Some pos); ty }
              | Error e -> Error (Some pos, e))
          | Some (FunEntry _) -> Error (Some pos, "function")
          | None -> Error (Some pos, "undefined variable"))
      | Tiger.FieldVar (var, field, _pos) -> walk_record var field
      | Tiger.SubscriptVar (_var, _exp, _pos) -> failwith "not implemented"
    in

    trexp exp

  let trans_prog (exp : Tiger.exp) =
    match trans_exp Env.base_venv Env.base_tenv exp with
    | Ok _ -> Ok ()
    | Error _ as err -> err
end
