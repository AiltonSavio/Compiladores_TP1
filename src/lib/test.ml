(* expect tests for the scanner *)

module L = Lexing

let scan_string s =
  let lexbuf = L.from_string s in
  let rec go () =
    let tok = Lexer.token lexbuf in
    Format.printf
      "%a %s\n%!"
      Location.pp_location (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  try go ()
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg

let%expect_test _ =
  scan_string "";
  [%expect{| :1.0-1.0 Parser.EOF |}];

  scan_string "   \t\t \n     \n  ";
  [%expect{| :3.2-3.2 Parser.EOF |}];

  scan_string "0 1342 56 32 98 139015 007";
  [%expect{|
           :1.0-1.1 (Parser.INTEGER 0)
           :1.2-1.6 (Parser.INTEGER 1342)
           :1.7-1.9 (Parser.INTEGER 56)
           :1.10-1.12 (Parser.INTEGER 32)
           :1.13-1.15 (Parser.INTEGER 98)
           :1.16-1.22 (Parser.INTEGER 139015)
           :1.23-1.26 (Parser.INTEGER 7)
           :1.26-1.26 Parser.EOF |}];

  scan_string "-4324";
  [%expect{| :1.0-1.1 error: illegal character '-' |}];

  scan_string "+12";
  [%expect{| :1.0-1.1 error: illegal character '+' |}];

  scan_string {|"a string literal" "another one"|};
  [%expect{|
    :1.0-1.18 (Parser.STRING "a string literal")
    :1.19-1.32 (Parser.STRING "another one")
    :1.32-1.32 Parser.EOF |}];

  scan_string {tigris|"with\tescape\tsequences"|tigris};
  [%expect{|
    :1.0-1.25 (Parser.STRING "with\tescape\tsequences")
    :1.25-1.25 Parser.EOF |}];

  scan_string {|"first line
                 second line
                 third line"|};
  [%expect{|
    :1.0-3.1 (Parser.STRING
       "first line\n                 second line\n                 third line")
    :3.1-3.1 Parser.EOF |}]
