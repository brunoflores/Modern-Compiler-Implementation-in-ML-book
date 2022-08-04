let rec lex_and_print lexbuf =
  try
    let token = Tigerlib.Lexer.read lexbuf in
    let out = Tigerlib.Token.string_of_token token in
    Printf.fprintf Out_channel.stdout "%s\n" out;
    match token with EOF -> () | _ -> lex_and_print lexbuf
  with Tigerlib.Lexer.SyntaxError s ->
    Printf.fprintf Out_channel.stderr "%s\n" s;
    exit 1

let get_contents = function
  | None | Some "-" ->
      In_channel.input_all In_channel.stdin |> Lexing.from_string
  | Some filename -> Stdio.In_channel.create filename |> Lexing.from_channel

let loop filename =
  let lexbuf = get_contents filename in
  lex_and_print lexbuf

let () =
  let usage = "Lex and display the tokens" in
  let filename = ref None in
  let spec = [] in
  let readfname fname =
    filename := if String.length fname > 0 then Some fname else None
  in
  Arg.parse spec readfname usage;
  loop !filename
