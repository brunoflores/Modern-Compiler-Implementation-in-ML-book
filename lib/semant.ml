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

  exception SemantError of (Tiger.pos option * string) list

  let find_record_field fields field pos =
    match List.find_opt (fun (x, _) -> x = field) fields with
    | Some (_, ty) -> { exp = Translate.Ex (Tree.Const 0); ty }
    | None ->
        raise
        @@ SemantError [ (Some pos, "field is not a member of the record") ]

  let ty_eq = function
    | Types.Int, Types.Int -> true
    | Types.String, Types.String -> true
    | _ -> false

  (* Find the actual type behind [Name]s. *)
  let rec actual_ty pos = function
    | ( Types.Int | Types.String | Types.Nil | Types.Unit | Types.Record _
      | Types.Array _ ) as t ->
        t
    | Types.Name (id, ty_opt) -> (
        let ty = !ty_opt in
        match ty with
        | Some ty -> actual_ty pos ty
        | None ->
            raise
            @@ SemantError
                 [
                   ( Some pos,
                     Format.sprintf "undefined type: %s" (Symbol.name id) );
                 ])

  let rec trans_decs venv tenv level decs : venv * tenv =
    match decs with
    | [] -> (venv, tenv)
    | dec :: decs' ->
        let venv', tenv' = trans_dec venv tenv level dec in
        trans_decs venv' tenv' level decs'

  and trans_dec venv tenv level dec : venv * tenv =
    match dec with
    | Tiger.VarDec { name; init; typ; pos; _ } ->
        let { ty; _ } = trans_exp venv tenv level init in
        (match typ with
        | Some (ty_assertion, pos) -> (
            match Symbol.look (tenv, ty_assertion) with
            | Some ty_assertion' -> (
                match ty_eq (ty_assertion', ty) with
                | true -> ()
                | false ->
                    raise
                    @@ SemantError
                         [ (Some pos, "type assertion does not hold") ])
            | None -> raise @@ SemantError [ (Some pos, "undefined type") ])
        | None -> ());
        ( Symbol.enter
            ( venv,
              name,
              Env.VarEntry
                { access = Translate.alloc_local level true; ty; pos } ),
          tenv )
    | Tiger.FunctionDec decs ->
        let trfundecs venv = function
          | [] -> venv
          | [ { Tiger.name; params; result; body; pos = decl_pos; _ } ] -> (
              match result with
              | Some (rt, rt_pos) -> (
                  match Symbol.look (tenv, rt) with
                  | Some rt ->
                      let transparam acc { Tiger.field_name; typ; field_pos; _ }
                          =
                        match Symbol.look (tenv, typ) with
                        | Some ty -> (field_name, ty) :: acc
                        | None ->
                            raise
                            @@ SemantError
                                 [ (Some field_pos, "undefined type") ]
                      in
                      let params' = List.fold_left transparam [] params in
                      let label = Temp.new_label () in
                      let venv' =
                        Symbol.enter
                          ( venv,
                            name,
                            Env.FunEntry
                              {
                                formals = List.map (fun (_, ty) -> ty) params';
                                result = rt;
                                level = Translate.new_level level label [ true ];
                                label;
                              } )
                      in
                      let enterparam venv (name, ty) =
                        Symbol.enter
                          ( venv,
                            name,
                            Env.VarEntry
                              {
                                access = Translate.alloc_local level true;
                                ty;
                                pos = decl_pos;
                              } )
                      in
                      let venv'' = List.fold_left enterparam venv' params' in
                      let _ = trans_exp venv'' tenv level body in
                      venv'
                  | None ->
                      raise
                      @@ SemantError
                           [
                             ( Some rt_pos,
                               Format.sprintf "undefined type: %s"
                                 (Symbol.name rt) );
                           ])
              | None -> failwith "not implemented")
          | _ -> failwith "not implemented"
        in
        let venv = trfundecs venv decs in
        (venv, tenv)
    | Tiger.TypeDec decs ->
        let rec trtydecs tenv = function
          | [] -> tenv
          | { Tiger.tydec_name; ty; _ } :: more ->
              let ty' = trans_ty tenv ty in
              trtydecs (Symbol.enter (tenv, tydec_name, ty')) more
        in
        let tenv' = trtydecs tenv decs in
        (venv, tenv')

  and trans_ty tenv ty : Types.ty =
    match ty with
    | Tiger.RecordTy fields ->
        let fields' =
          List.fold_left
            (fun acc { Tiger.field_name; typ; field_pos; _ } ->
              match Symbol.look (tenv, typ) with
              | Some f -> (field_name, f) :: acc
              | None ->
                  raise
                  @@ SemantError
                       [
                         ( Some field_pos,
                           Format.sprintf "undefined type: %s" (Symbol.name typ)
                         );
                       ])
            [] fields
        in
        let fs = fields' in
        Types.Record (fs, ref ())
    | Tiger.NameTy (sym, _pos) -> (
        match Symbol.look (tenv, sym) with
        | Some _ as ty -> Types.Name (sym, ref ty)
        | None -> Types.Name (sym, ref None))
    | _ -> failwith "not implemented"

  and trans_exp venv tenv level exp : expty =
    let rec actual_seq_ty (exps : (Tiger.exp * Tiger.pos) list) : expty =
      match exps with
      | [] -> { exp = Translate.Ex (Tree.Const 0); ty = Types.Unit }
      | [ (exp, _pos) ] -> trexp exp
      | (exp, pos) :: exps' -> (
          match trexp exp with
          | { ty = Types.Unit; _ } -> actual_seq_ty exps'
          | { ty = _; _ } ->
              raise
              @@ SemantError
                   [
                     ( Some pos,
                       "this expression is on the left side of a sequence: \
                        expected unit" );
                   ])
    and trexp = function
      | NilExp -> { exp = Translate.Ex (Tree.Const 0); ty = Types.Nil }
      | StringExp (_, _pos) ->
          { exp = Translate.Ex (Tree.Const 0); ty = Types.String }
      | IntExp _ -> { exp = Translate.Ex (Tree.Const 0); ty = Types.Int }
      | SeqExp exps -> actual_seq_ty exps
      | VarExp var -> trvar var
      | LetExp { decs; body; _ } ->
          let venv', tenv' = trans_decs venv tenv level decs in
          trans_exp venv' tenv' level body
      | OpExp { left; oper = PlusOp; right; pos }
      | OpExp { left; oper = DivideOp; right; pos }
      | OpExp { left; oper = TimesOp; right; pos }
      | OpExp { left; oper = MinusOp; right; pos } -> (
          let trintexp e =
            match trexp e with
            | { ty = Types.Int; _ } ->
                { exp = Translate.Ex (Tree.Const 0); ty = Types.Int }
            | _ -> raise @@ SemantError [ (None, "") ]
            (* TODO *)
          in
          try
            let _ = trintexp left in
            let _ = trintexp right in
            { exp = Translate.Ex (Tree.Const 0); ty = Types.Int }
          with SemantError _ ->
            raise
            @@ SemantError [ (Some pos, "this operation requires two ints") ])
      | OpExp { left; oper = GeOp; right; pos }
      | OpExp { left; oper = GtOp; right; pos }
      | OpExp { left; oper = LeOp; right; pos }
      | OpExp { left; oper = LtOp; right; pos }
      | OpExp { left; oper = NeqOp; right; pos }
      | OpExp { left; oper = EqOp; right; pos } -> (
          match (trexp left, trexp right) with
          | { ty = Types.String; _ }, { ty = Types.String; _ } ->
              { exp = Translate.Ex (Tree.Const 0); ty = Types.String }
          | { ty = Types.Int; _ }, { ty = Types.Int; _ } ->
              { exp = Translate.Ex (Tree.Const 0); ty = Types.Int }
          | _ ->
              raise
              @@ SemantError [ (Some pos, "both sides must be string or int") ])
      | ArrayExp { typ; size; init; pos } -> (
          match Symbol.look (tenv, typ) with
          | Some (Types.Array (array_ty, _)) -> (
              let _ =
                match trexp size with
                | { ty = Types.Int; _ } -> ()
                | _ -> raise @@ SemantError [ (Some pos, "integer required") ]
              in
              let { ty; _ } = trexp init in
              match ty_eq (array_ty, ty) with
              | true ->
                  {
                    exp = Translate.Ex (Tree.Const 0);
                    ty = Types.Array (ty, ref ());
                  }
              | false ->
                  raise
                  @@ SemantError
                       [
                         ( Some pos,
                           "type of array initialisation value does not match \
                            the array declaration" );
                       ])
          | _ ->
              raise @@ SemantError [ (Some pos, "type not declared as array") ])
      | RecordExp { typ; pos; fields = arg_fields } -> (
          match Symbol.look (tenv, typ) with
          | Some (Types.Record (formal_fields, _)) -> (
              let exps =
                List.fold_left
                  (fun (errs, oks) (sym, exp, pos) ->
                    try
                      let ok = trexp exp in
                      (errs, (sym, ok, pos) :: oks)
                    with err -> (err :: errs, oks))
                  ([], []) arg_fields
              in
              match exps with
              | err :: _, _ -> raise err
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
                      raise
                      @@ SemantError
                           [
                             ( Some pos,
                               Format.sprintf "field type mismatch: %s"
                                 (Symbol.name sym) );
                           ]
                  | None ->
                      let tys =
                        List.map
                          (fun (_, sym, { ty; _ }, _pos) -> (sym, ty))
                          tests
                      in
                      {
                        exp = Translate.Ex (Tree.Const 0);
                        ty = Types.Record (tys, ref ());
                      }))
          | _ ->
              raise
              @@ SemantError
                   [
                     ( Some pos,
                       Format.sprintf "record type undefined: %s"
                         (Symbol.name typ) );
                   ])
      | CallExp { func; args; pos } -> (
          let args =
            List.fold_left
              (fun acc (exp, pos) ->
                let { ty; _ } = trexp exp in
                (ty, pos) :: acc)
              [] args
          in
          match Symbol.look (venv, func) with
          | Some (Env.FunEntry { formals; result; level = _; label = _ }) ->
              let _ =
                List.iter2
                  (fun (arg, _) form ->
                    match ty_eq (arg, form) with
                    | true -> ()
                    | false ->
                        raise
                        @@ SemantError [ (Some pos, "argument type mismatch") ])
                  args formals
              in
              { exp = Translate.Ex (Tree.Const 0); ty = result }
          | Some _ ->
              raise
              @@ SemantError [ (Some pos, "symbol not bound to a function") ]
          | None -> raise @@ SemantError [ (Some pos, "undefined function") ])
      | AssignExp _ | IfExp _ | WhileExp _ | ForExp _ | BreakExp _ ->
          failwith "not implemented"
    and trvar var : expty =
      let rec walk_record (field : Symbol.symbol) pos = function
        | Tiger.SimpleVar (id, _pos) -> (
            match Symbol.look (venv, id) with
            | Some (Env.VarEntry { ty = Types.Record (fields, _); _ }) ->
                find_record_field fields field pos
            | Some _ ->
                raise @@ SemantError [ (Some pos, "var is not a record") ]
            | None -> raise @@ SemantError [ (Some pos, "undefined record") ])
        | Tiger.SubscriptVar _ as var -> (
            match trvar var with
            | { ty = Types.Record (fields, _); _ } ->
                find_record_field fields field pos
            | { ty = _; _ } ->
                raise
                @@ SemantError [ (Some pos, "this array item is not a record") ]
            )
        | Tiger.FieldVar (var, field, pos) -> walk_record field pos var
      in
      match var with
      | Tiger.SimpleVar (id, pos) -> (
          match Symbol.look (venv, id) with
          | Some (Env.VarEntry { ty; _ }) ->
              let ty = actual_ty pos ty in
              { exp = Translate.Ex (Tree.Const 0); ty }
          | Some (FunEntry _) -> raise @@ SemantError [ (Some pos, "function") ]
          | None ->
              raise
              @@ SemantError
                   [
                     ( Some pos,
                       Format.sprintf "undefined variable: %s" (Symbol.name id)
                     );
                   ])
      | Tiger.FieldVar (var, field, pos) -> walk_record field pos var
      | Tiger.SubscriptVar (var, exp, pos) -> (
          match trvar var with
          | { ty = Types.Array (arr_ty, _); _ } ->
              let _ =
                match trexp exp with
                | { ty = Types.Int; _ } -> ()
                | _ -> raise @@ SemantError [ (Some pos, "integer required") ]
              in
              { exp = Translate.Ex (Tree.Const 0); ty = arr_ty }
          | { ty = _; _ } ->
              raise
              @@ SemantError
                   [ (Some pos, "taking a subscript here: expected an array") ])
    in
    trexp exp

  let trans_prog exp =
    try
      let { exp; _ } =
        trans_exp Env.base_venv Env.base_tenv Translate.outermost exp
      in
      print_endline @@ Translate.show_exp exp;
      Ok ()
    with SemantError errs -> Error errs
end
