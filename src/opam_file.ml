open! Stdune
open Import
open OpamParserTypes

type t = opamfile

let parse (lb : Lexing.lexbuf) =
  try
    OpamBaseParser.main OpamLexer.token lb lb.lex_curr_p.pos_fname
  with
  | OpamLexer.Error msg ->
    Errors.fail_lex lb "%s" msg
  | Parsing.Parse_error ->
    Errors.fail_lex lb "Parse error"

let load fn =
  Io.with_lexbuf_from_file fn ~f:parse

let get_field t name =
  List.find_map t.file_contents
    ~f:(function
      | Variable (_, var, value) when name = var ->
        Some value
      | _ -> None)
