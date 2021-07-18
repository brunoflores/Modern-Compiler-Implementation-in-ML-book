let print_position outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let fname = pos.pos_fname in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  fprintf outx "%s:%d:%d" fname line col

let parse_with_error lexbuf =
  try Tigerlib.Parser.prog Tigerlib.Lexer.read lexbuf with
  | Tigerlib.Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Tigerlib.Parser.Error ->
      fprintf stderr "%a: Syntax error\n" print_position lexbuf;
      exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      let output = Format.asprintf "%a" Tigerlib.Tiger.pp_exp value in
      fprintf stdout "%s\n" output;
      parse_and_print lexbuf
  | None -> ()

let get_contents = function
  | None | Some "-" ->
      In_channel.input_all In_channel.stdin |> Lexing.from_string
  | Some filename -> In_channel.create filename |> Lexing.from_channel

let loop filename =
  let lexbuf = get_contents filename in
  parse_and_print lexbuf

let command =
  Command.basic ~summary:"Parse and display the AST"
    Command.Let_syntax.(
      let%map_open filename = anon (maybe ("filename" %: Filename.arg_type)) in
      fun () -> loop filename)

let () = Command.run command
