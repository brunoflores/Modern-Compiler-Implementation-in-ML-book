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

  (* Helper type to specify the valid types for binary operations. *)
  type binop_ty = String | Int

  (* Helper *)
  let find_record_field fields field pos =
    match List.find_opt (fun (x, _) -> x = field) fields with
    | Some (_, ty) -> Ok { exp = ((), None); ty }
    | None -> Error (Some pos, "field is not a member of the record")

  (* Helper *)
  let ty_eq = function Types.Int, Types.Int -> true | _ -> false

  (* Helper *)
  (* Find the actual type behind [Name]s. *)
  let rec actual_ty = function
    | ( Types.Int | Types.String | Types.Nil | Types.Unit | Types.Record _
      | Types.Array _ ) as t ->
        Ok t
    | Types.Name (id, ty_opt) -> (
        let ty = !ty_opt in
        match ty with
        | Some ty -> actual_ty ty
        | None -> Error (Format.sprintf "undefined type: %s" (Symbol.name id)))

  let rec trans_decs (venv : venv) (tenv : tenv) (decs : Tiger.dec list) :
      (venv * tenv, error) result =
    match decs with
    | [] -> Ok (venv, tenv)
    | dec :: decs' -> (
        match trans_dec venv tenv dec with
        | Ok (venv', tenv') -> trans_decs venv' tenv' decs'
        | Error _ as err -> err)

  and trans_dec (venv : venv) (tenv : tenv) (dec : Tiger.dec) :
      (venv * tenv, error) result =
    match dec with
    | Tiger.VarDec { name; init; typ; _ } -> (
        match trans_exp venv tenv init with
        | Ok { ty; _ } -> (
            let assertion_test =
              match typ with
              | Some (ty_assertion, pos) -> (
                  match Symbol.look (tenv, ty_assertion) with
                  | Some ty_assertion' -> (
                      match ty_eq (ty_assertion', ty) with
                      | true -> Ok ()
                      | false -> Error (Some pos, "type assertion does not hold")
                      )
                  | None -> Error (Some pos, "undefined type"))
              | None -> Ok ()
            in
            match assertion_test with
            | Ok () ->
                Ok
                  ( Symbol.enter
                      ( venv,
                        name,
                        Env.VarEntry
                          { (* access = Translate.alloc_local (); *) ty } ),
                    tenv )
            | Error _ as err -> err)
        | Error _ as err -> err)
    | Tiger.FunctionDec _ -> failwith "not implemented"
    | Tiger.TypeDec decs -> (
        let rec trtydecs tenv = function
          | [] -> Ok tenv
          | { Tiger.tydec_name; ty; _ } :: more -> (
              match trans_ty tenv ty with
              | Ok ty' -> trtydecs (Symbol.enter (tenv, tydec_name, ty')) more
              | Error _ as err -> err)
        in
        match trtydecs tenv decs with
        | Ok tenv' -> Ok (venv, tenv')
        | Error _ as err -> err)

  and trans_ty (tenv : tenv) (ty : Tiger.ty) : (Types.ty, error) result =
    match ty with
    | Tiger.RecordTy fields -> (
        let fields' =
          List.fold_left
            (fun acc { Tiger.field_name; typ; field_pos; _ } ->
              match acc with
              | Error _ -> acc
              | Ok acc -> (
                  match Symbol.look (tenv, typ) with
                  | Some f -> Ok ((field_name, f) :: acc)
                  | None ->
                      Error
                        ( Some field_pos,
                          Format.sprintf "undefined type: %s" (Symbol.name typ)
                        )))
            (Ok []) fields
        in
        match fields' with
        | Ok fs -> Ok (Types.Record (fs, ref ()))
        | Error _ as err -> err)
    | Tiger.NameTy (sym, _pos) -> (
        match Symbol.look (tenv, sym) with
        | Some _ as ty -> Ok (Types.Name (sym, ref ty))
        | None -> Ok (Types.Name (sym, ref None)))
    | _ -> failwith "not implemented"

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
      | Tiger.LetExp { decs; body; _ } -> (
          match trans_decs venv tenv decs with
          | Ok (venv', tenv') -> trans_exp venv' tenv' body
          | Error _ as err -> err)
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
      | Tiger.ArrayExp { typ; size; init; pos } -> (
          match Symbol.look (tenv, typ) with
          | Some (Types.Array (array_ty, _)) -> (
              match check_int (trexp size) with
              | Ok _ -> (
                  match trexp init with
                  | Ok { ty; _ } -> (
                      match ty_eq (array_ty, ty) with
                      | true ->
                          Ok { exp = ((), None); ty = Types.Array (ty, ref ()) }
                      | false ->
                          Error
                            ( Some pos,
                              "type of array initialisation value does not \
                               match the array declaration" ))
                  | Error _ as err -> err)
              | Error _ as err -> err)
          | _ -> Error (Some pos, "type not delclared as array"))
      | Tiger.RecordExp { typ; pos; fields = arg_fields } -> (
          match Symbol.look (tenv, typ) with
          | Some (Types.Record (formal_fields, _)) -> (
              let exps =
                List.fold_left
                  (fun (errs, oks) (sym, exp, pos) ->
                    match trexp exp with
                    | Ok ok -> (errs, (sym, ok, pos) :: oks)
                    | Error err -> (err :: errs, oks))
                  ([], []) arg_fields
              in
              match exps with
              | err :: _, _ -> Error err
              | _, fields -> (
                  let tests =
                    List.fold_left
                      (fun acc (sym, expty, pos) ->
                        let { ty; _ } = expty in
                        match
                          List.find_opt
                            (fun (s, _) -> Symbol.equal sym s)
                            formal_fields
                        with
                        | Some (_, formal_ty) -> (
                            match ty_eq (ty, formal_ty) with
                            | false -> (false, sym, expty, pos) :: acc
                            | true -> (true, sym, expty, pos) :: acc)
                        | None -> (false, sym, expty, pos) :: acc)
                      [] fields
                  in
                  match
                    List.find_opt
                      (fun (result, _, _, _) -> result = false)
                      tests
                  with
                  | Some (_, sym, _, pos) ->
                      Error
                        ( Some pos,
                          Format.sprintf "field type mismatch: %s"
                            (Symbol.name sym) )
                  | None ->
                      let tys =
                        List.map
                          (fun (_, sym, { ty; _ }, _pos) -> (sym, ty))
                          tests
                      in
                      Ok { exp = ((), None); ty = Types.Record (tys, ref ()) }))
          | _ ->
              Error
                ( Some pos,
                  Format.sprintf "record type undefined: %s" (Symbol.name typ)
                ))
      | Tiger.CallExp _ | Tiger.AssignExp _ | Tiger.IfExp _ | Tiger.WhileExp _
      | Tiger.ForExp _ | Tiger.BreakExp _ ->
          failwith "not implemented"
    and trvar (var : Tiger.var) : (expty, error) result =
      let rec walk_record (var : Tiger.var) (field : Symbol.symbol) pos :
          (expty, error) result =
        match var with
        | Tiger.SimpleVar (id, _pos) -> (
            match Symbol.look (venv, id) with
            | Some (Env.VarEntry { ty = Types.Record (fields, _); _ }) ->
                find_record_field fields field pos
            | Some _ -> Error (Some pos, "var is not a record")
            | None -> Error (Some pos, "undefined record"))
        | Tiger.SubscriptVar _ as var -> (
            match trvar var with
            | Ok { ty = Types.Record (fields, _); _ } ->
                find_record_field fields field pos
            | Ok { ty = _; _ } ->
                Error (Some pos, "this array item is not a record")
            | Error _ as err -> err)
        | Tiger.FieldVar (var, field, pos) -> walk_record var field pos
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
          | None ->
              Error
                ( Some pos,
                  Format.sprintf "undefined variable: %s" (Symbol.name id) ))
      | Tiger.FieldVar (var, field, pos) -> walk_record var field pos
      | Tiger.SubscriptVar (var, exp, pos) -> (
          match trvar var with
          | Ok { ty = Types.Array (arr_ty, _); _ } -> (
              match check_int (trexp exp) with
              | Ok _ -> Ok { exp = ((), None); ty = arr_ty }
              | Error _ as err -> err)
          | Ok { ty = _; _ } ->
              Error (Some pos, "taking a subscript here: expected an array")
          | Error _ as err -> err)
    in

    trexp exp

  let trans_prog (exp : Tiger.exp) =
    match trans_exp Env.base_venv Env.base_tenv exp with
    | Ok _ -> Ok ()
    | Error _ as err -> err
end
