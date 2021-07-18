open Core

let rec lex_and_print lexbuf =
  try
    let token = Tigerlib.Lexer.read lexbuf in
    let out = Tigerlib.Token.string_of_token token in
    Printf.fprintf stdout "%s\n" out;
    match token with EOF -> () | _ -> lex_and_print lexbuf
  with Tigerlib.Lexer.SyntaxError s ->
    Printf.fprintf stderr "%s\n" s;
    exit 1

let get_contents = function
  | None | Some "-" ->
      In_channel.input_all In_channel.stdin |> Lexing.from_string
  | Some filename -> In_channel.create filename |> Lexing.from_channel

let loop filename =
  let lexbuf = get_contents filename in
  lex_and_print lexbuf

let command =
  Command.basic ~summary:"Lex and display the tokens"
    Command.Let_syntax.(
      let%map_open filename = anon (maybe ("filename" %: Filename.arg_type)) in
      fun () -> loop filename)

let () = Command.run command
