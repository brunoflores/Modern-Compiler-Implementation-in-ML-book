module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Tigerlib.Parser.MenhirInterpreter

let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let fname = pos.pos_fname in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.fprintf outx "%s:%d:%d" fname line col

let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0

let show text positions =
  E.extract text positions |> E.sanitize |> E.compress |> E.shorten 20
(* max width 43 *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) -> show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

let succeed v =
  match v with
  | Some x -> Format.printf "%a\n" Tigerlib.Tiger.pp_exp x
  | None -> ()

let fail text buffer checkpoint =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication =
    Format.sprintf "Syntax error %s.\n" (E.show (show text) buffer)
  in
  (* Fetch an error message from the database. *)
  let message = Tigerlib.ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  Printf.eprintf "%s%s%s%!" location indication message;
  exit 1

let parse lexbuf text =
  let supplier = I.lexer_lexbuf_to_supplier Tigerlib.Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Tigerlib.Parser.Incremental.prog lexbuf.lex_curr_p in
  try I.loop_handle succeed (fail text buffer) supplier checkpoint
  with Tigerlib.Lexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    ()

(* try Parser.prog Lexer.read lexbuf with
   | Lexer.SyntaxError msg ->
       fprintf stderr "%a: %s\n" print_position lexbuf msg;
       None
   | Parser.Error ->
       fprintf stderr "%a: Syntax error\n" print_position lexbuf;
       exit (-1) *)

(* let parse_and_print lexbuf content =
   match parse lexbuf content with
   | Some value -> fprintf stdout "%s\n" value
   | None -> () *)

let get_contents s =
  let filename, content =
    match s with
    | None | Some "-" -> ("-", In_channel.input_all In_channel.stdin)
    | Some filename -> (filename, Core.In_channel.read_all filename)
  in
  (L.init filename (content |> Lexing.from_string), content)

let loop filename =
  let lexbuf, content = get_contents filename in
  (* parse_and_print lexbuf content *)
  parse lexbuf content

let () =
  let usage = "Parse and display the AST" in
  let filename = ref None in
  let spec = [] in
  let readfname fname =
    filename := if String.length fname > 0 then Some fname else None
  in
  Arg.parse spec readfname usage;
  loop !filename
