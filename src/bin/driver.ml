(* driver *)

module L = Lexing

(* get and show all tokens from lexical buffer *)
let scan lexbuf =
  (* an auxiliary local recursive function to loop *)
  let rec go () =
    (* extract the next token *)
    let tok = Lexer.token lexbuf in
    (* show the extracted token *)
    Format.printf
      "%a %s\n"
      Location.pp_location (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    (* check if it is end of file *)
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  (* calls the auxiliary function to start looping *)
  go ()

let main () =
  (* turns on recording of exception backtraces for debugging *)
  Printexc.record_backtrace true;

  (* parse command line arguments *)
  Cmdline.parse_cmdline ();

  (* create lexical buffer *)
  let lexbuf = L.from_channel (Cmdline.get_input_channel ()) in
  Lexer.set_filename lexbuf (Cmdline.get_input_file_name ());

  try
    (* should the tokens be printed? *)
    if Cmdline.get_show_tokens () then
      scan lexbuf
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg;
     exit 1

(* calls the main function *)
let () = main ()
