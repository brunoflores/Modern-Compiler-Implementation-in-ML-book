module type S = sig
  val trans_prog : Tiger.exp -> (unit, (Tiger.pos option * string) list) result
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
  type errors = error list
  type ty_res = (expty, errors) result
  type binop_ty = String | Int

  let rec trans_exp (venv : venv) (tenv : tenv) (exp : Tiger.exp) : ty_res =
    let rec check_int (e : ty_res) =
      match e with
      | Ok expty -> (
          match expty.ty with
          | Types.Int -> e
          | _ -> Error [ (None, "integer required") ])
      | Error _ -> e
    and check_int_binop (errs : errors) left right : (unit, errors) result =
      let lefty = check_int (trexp errs left) in
      let righty = check_int (trexp errs right) in
      match (lefty, righty) with
      | Ok _, Ok _ -> Ok ()
      | Error err', Error err'' -> Error (err' @ err'' @ errs)
      | _, Error err' | Error err', _ -> Error (err' @ errs)
    and check_binop errs left right pos : (binop_ty, errors) result =
      let lefty = trexp errs left in
      let righty = trexp errs right in
      match (lefty, righty) with
      | Ok { ty = Types.String; _ }, Ok { ty = Types.String; _ } -> Ok String
      | Ok { ty = Types.Int; _ }, Ok { ty = Types.Int; _ } -> Ok Int
      | _ -> Error ((Some pos, "both sides must be string or int") :: errs)
    and trexp (errs : errors) (e : Tiger.exp) : ty_res =
      match e with
      | Tiger.NilExp -> Ok { exp = ((), None); ty = Types.Nil }
      | Tiger.StringExp (_, pos) ->
          Ok { exp = ((), Some pos); ty = Types.String }
      | Tiger.IntExp _ -> Ok { exp = ((), None); ty = Types.Int }
      | Tiger.SeqExp [] -> failwith ""
      | Tiger.SeqExp ((e, _) :: _) -> trexp errs e
      | Tiger.VarExp (Tiger.SimpleVar (x, pos)) -> (
          match Symbol.look (venv, x) with
          | Some (Env.VarEntry { ty; _ }) -> Ok { exp = ((), Some pos); ty }
          | Some (FunEntry _) -> failwith "not implemented"
          | None ->
              Error
                (( Some pos,
                   Format.asprintf "variable %S undefined" (Symbol.name x) )
                :: errs))
      | Tiger.LetExp { decs; body; _ } ->
          let venv, tenv = trdec errs decs in
          trans_exp venv tenv body
      | Tiger.OpExp { left; oper = Tiger.PlusOp; right; _ }
      | Tiger.OpExp { left; oper = Tiger.DivideOp; right; _ }
      | Tiger.OpExp { left; oper = Tiger.TimesOp; right; _ }
      | Tiger.OpExp { left; oper = Tiger.MinusOp; right; _ } -> (
          match check_int_binop errs left right with
          | Ok _ -> Ok { exp = ((), None); ty = Types.Int }
          | Error _ as err -> err)
      | Tiger.OpExp { left; oper = Tiger.GeOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.GtOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.LeOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.LtOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.NeqOp; right; pos }
      | Tiger.OpExp { left; oper = Tiger.EqOp; right; pos } -> (
          match check_binop errs left right pos with
          | Ok String -> Ok { exp = ((), Some pos); ty = Types.String }
          | Ok Int -> Ok { exp = ((), Some pos); ty = Types.Int }
          | Error _ as err -> err)
      | Tiger.VarExp var -> trvar errs var
      | Tiger.CallExp _ | Tiger.RecordExp _ | Tiger.AssignExp _ | Tiger.IfExp _
      | Tiger.WhileExp _ | Tiger.ForExp _ | Tiger.BreakExp _ | Tiger.ArrayExp _
        ->
          failwith ""
    and trdec (errs : errors) (decs : Tiger.dec list) =
      let venv =
        List.map
          (function
            | Tiger.VarDec { name; init; _ } -> (
                match trexp errs init with
                | Ok { ty; _ } ->
                    ( Env.VarEntry { access = Translate.alloc_local (); ty },
                      name )
                | Error _ -> failwith "FATAL")
            | Tiger.FunctionDec _ | Tiger.TypeDec _ ->
                failwith "not implemented")
          decs
        |> List.fold_left
             (fun acc (x, name) -> Symbol.enter (acc, name, x))
             venv
      in
      (venv, tenv)
    and trvar (errs : errors) (var : Tiger.var) : ty_res =
      match var with
      | Tiger.SimpleVar (id, pos) -> (
          match Symbol.look (venv, id) with
          | Some (Env.VarEntry _) ->
              Ok { exp = ((), Some pos); ty = Types.Name (id, ref None) }
          | Some (FunEntry _) | None ->
              Error ((Some pos, "undefined variable") :: errs))
      | Tiger.FieldVar (obj, field, _) -> (
          match obj with
          | Tiger.SimpleVar (id, pos) -> (
              match Symbol.look (venv, id) with
              | Some (Env.VarEntry { ty = Types.Record (fields, _); _ }) -> (
                  match List.find_opt (fun (x, _) -> x = field) fields with
                  | Some _ -> Ok { exp = ((), Some pos); ty = Types.Int }
                  | None ->
                      Error
                        ((Some pos, "field is not a member of the record")
                        :: errs))
              | Some _ -> failwith ""
              | None -> Error ((Some pos, "undefined record") :: errs))
          | Tiger.FieldVar _ -> failwith ""
          | Tiger.SubscriptVar _ -> failwith "")
      | Tiger.SubscriptVar _ -> failwith "not implemented"
    in

    trexp [] exp

  let trans_prog (exp : Tiger.exp) =
    match trans_exp Env.base_venv Env.base_tenv exp with
    | Ok _ -> Ok ()
    | Error _ as err -> err
end
