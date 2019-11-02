(* handling command line arguments: interface *)

val get_input_file_name : unit -> string
val get_input_channel : unit -> in_channel
val get_show_tokens : unit -> bool

val parse_cmdline : unit -> unit
