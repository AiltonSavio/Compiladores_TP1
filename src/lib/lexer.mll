{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let unterminated_comment loc =
    Error.error loc "unterminated comment"

  let unterminated_string loc =
    Error.error loc "unterminated string"

  let illegal_escape loc sequence =
    Error.error loc "illegal escape sequence '%s' in string literal" sequence

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }

  (* a string buffer to accumulate characters when scanning string literals *)
  let string_buffer = Buffer.create 16

  (* helper function to update new line counting while scanning string literals *)
  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> L.new_line lexbuf | _ -> ()) str
}

let spaces = [' ' '\t'] +
let digit = ['0'-'9']
let litint = digit +
let logic = ("false" | "true")
let real = ((digit+ '.' digit* ) 
          | (digit* '.' digit+ (['e' 'E'] ['+' '-']? digit+)?) 
          | (digit+ ['e' 'E'] digit+))
let id = ['a'-'z' 'A'-'Z']+ (['a'-'z' 'A'-'Z'] | digit | '_')*

let control = ['A'-'Z' '@' '[' '\\' ']' '^' '_']

rule token = parse
  | spaces        { token lexbuf }
  | '\n'          { L.new_line lexbuf;
                    token lexbuf }
  | litint as lxm { INTEGER (int_of_string lxm) }
  | '"'           { string lexbuf.L.lex_start_p lexbuf }
  | '#'           { line_comment lexbuf }
  | "{#"          { block_comment [lexbuf.L.lex_start_p] lexbuf }
  | logic as lxm  { LOGIC (bool_of_string lxm) }
  | real as lxm   { REAL (float_of_string lxm) }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "while"       { WHILE }
  | "do"          { DO }
  | "break"       { BREAK }
  | "let"         { LET }
  | "in"          { IN }
  | "end"         { END }
  | "var"         { VAR }
  | id as lxm     { ID (Symbol.symbol lxm) }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | ':'           { COLON }
  | ','           { COMMA }
  | ';'           { SEMI }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { TIMES }
  | '/'           { DIV }
  | '%'           { MOD }
  | '^'           { POW }
  | '='           { EQ }
  | "<>"          { NE }
  | '<'           { LT }
  | "<="          { LE }
  | '>'           { GT }
  | ">="          { GE }
  | '&'           { AND }
  | '|'           { OR }
  | ":="          { ASSIGN }
  | eof           { EOF }
  | _             { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }

and line_comment = parse
    | "\n" {  token lexbuf}
    | eof  { unterminated_comment (Location.curr_loc lexbuf) }
    | _ { line_comment lexbuf }

and block_comment level = parse
  | "#}" { match level with
           | [_] -> token lexbuf
           | _::level' -> block_comment level' lexbuf
           | [] -> failwith "bug in comment scanner"
         }
  | "{#" { block_comment (lexbuf.L.lex_start_p :: level) lexbuf }
  | '\n' { L.new_line lexbuf;
           block_comment level lexbuf
         }
  | eof  { unterminated_comment (List.hd level, lexbuf.L.lex_start_p);
           token lexbuf
         }
  | _    { block_comment level lexbuf }


and string pos = parse
  | '"'                  { lexbuf.L.lex_start_p <- pos;
                          let text = Buffer.contents string_buffer in
                          Buffer.clear string_buffer;
                          STRING text }
  | "\\t"                { Buffer.add_char string_buffer '\t';
                          string pos lexbuf }
  | "\\\\"               { Buffer.add_char string_buffer '\\';
                          string pos lexbuf }
  | "\\\""               { Buffer.add_char string_buffer '\"';
                          string pos lexbuf } 
  | "\\v"                 { Buffer.add_char string_buffer '\011';
                          string pos lexbuf }
  | "\\n"                 { Buffer.add_char string_buffer '\n';
                          string pos lexbuf }
  | "\\r"                 { Buffer.add_char string_buffer '\r';
                          string pos lexbuf }                      
  | "\\b"                 { Buffer.add_char string_buffer '\b';
                          string pos lexbuf }
  | "\\f"                 { Buffer.add_char string_buffer '\012';
                          string pos lexbuf }
  | "\\^" control         { Buffer.add_char string_buffer '\000';
                          string pos lexbuf }
  | "\\^?"                { Buffer.add_char string_buffer '\127';
                          string pos lexbuf }
  | "\\" digit digit digit { Buffer.add_char string_buffer '\001';
                          string pos lexbuf }
  | "\\" _ as x           { illegal_escape (lexbuf.L.lex_start_p, lexbuf.L.lex_curr_p) x;
                          string pos lexbuf}
  | [^ '\\' '"']+ as lxm  { str_incr_linenum lxm lexbuf;
                          Buffer.add_string string_buffer lxm;
                          string pos lexbuf }
  | eof                   { unterminated_string (pos, lexbuf.L.lex_start_p);
                          token lexbuf }
