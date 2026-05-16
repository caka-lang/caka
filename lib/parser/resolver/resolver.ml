open Ast
open Utils

let rec resolve code =
  let buf = Sedlexing.Utf8.from_string code in
  Sedlexing.set_filename buf "main";
  let tokenize, lexbuf = Lexer.tokenize buf in
  let result =
    try lexbuf |> Parser.start tokenize
    with Parser.Error ->
      let start_p = lexbuf.lex_start_p in
      let fname = start_p.pos_fname in
      let lnum = start_p.pos_lnum in
      let cnum = start_p.pos_cnum - start_p.pos_bol in
      raise
      @@ Errors.Error (Printf.sprintf "%s:%d:%d: syntax error" fname lnum cnum)
  in
  (* let env = EnvManager.create in *)
  result
