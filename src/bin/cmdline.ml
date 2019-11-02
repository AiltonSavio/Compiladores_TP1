(* handling command line arguments: implementation *)

let input_file_name = ref ""
let input_channel = ref stdin
let show_tokens = ref true

let get_input_file_name () = !input_file_name
let get_input_channel () = !input_channel
let get_show_tokens () = !show_tokens

let set_input s =
  try
    input_file_name := s;
    input_channel := open_in s
  with Sys_error err ->
    raise (Arg.Bad ("Cannot open '" ^ s ^ "': " ^ err))

let usage_msg =
  "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... FILE\n"

let rec usage () =
  Arg.usage options usage_msg;
  exit 0

and options =
  [ "-lex",    Arg.Set show_tokens,    "\tDisplay sequence of lexical symbols"
  ; "-help",   Arg.Unit usage, "\tDisplay this list of options"
  ; "--help",  Arg.Unit usage, "\tDisplay this list of options"
  ]

let parse_cmdline () =
  Arg.parse options set_input usage_msg
