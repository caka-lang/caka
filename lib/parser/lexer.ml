open Parser

exception Error of string * Lexing.position

let last_token_was_end_of_expr = ref false
let nesting_level = ref 0

let reset_state () =
  last_token_was_end_of_expr := false;
  nesting_level := 0

let expr_end tok =
  last_token_was_end_of_expr := true;
  tok

let expr_cont tok =
  last_token_was_end_of_expr := false;
  tok

let open_paren tok =
  incr nesting_level;
  expr_cont tok

let close_paren tok =
  decr nesting_level;
  expr_end tok

let digit = [%sedlex.regexp? '0' .. '9']
let float = [%sedlex.regexp? Plus digit, '.', Plus digit]
let ident = [%sedlex.regexp? alphabetic, Star (alphabetic | digit | '_')]
let whitespace = [%sedlex.regexp? ' ' | '\t' | '\r']

let rec token buf =
  match%sedlex buf with
  | Plus whitespace -> token buf
  | '\n' -> NEWLINE
  | Plus digit -> INT (int_of_string (Sedlexing.Utf8.lexeme buf))
  | float -> FLOAT (float_of_string (Sedlexing.Utf8.lexeme buf))
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> STAR
  | '/' -> SLASH
  | '%' -> MOD
  | '^' -> POW
  | '&' -> AMPAND
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | '=' -> EQ
  | ',' -> COMMA
  | '.' -> DOT
  | ':' -> COLON
  | "==" -> EQEQ
  | "!=" -> NE
  | "<" -> LT
  | "<=" -> LTE
  | ">" -> GT
  | ">=" -> GTE
  | "nil" -> NIL
  | "extern" -> EXTERN
  | "return" -> RETURN
  | "type" -> TYPE
  | "struct" -> STRUCT
  | "void" -> TVOID
  | "bool" -> TBOOL
  | "string" -> TSTRING
  | '"' -> read_string (Buffer.create 8) buf
  | "int" -> TINT
  | "i8" -> TINT8
  | "i16" -> TINT16
  | "i32" -> TINT32
  | "i64" -> TINT64
  | "i128" -> TINT128
  | "uint" -> TUINT
  | "u8" -> TUINT8
  | "u16" -> TUINT16
  | "u32" -> TUINT32
  | "u64" -> TUINT64
  | "u128" -> TUINT128
  | "float" -> TFLOAT
  | "f32" -> TFLOAT32
  | "f64" -> TFLOAT64
  | "true" -> TRUE
  | "false" -> FALSE
  | eof -> EOF
  | ident -> IDENT (Sedlexing.Utf8.lexeme buf)
  | any ->
      let start_p, _ = Sedlexing.lexing_positions buf in
      let col = start_p.pos_cnum - start_p.pos_bol in
      failwith
        (Printf.sprintf "%s:%d:%d: unexpected character: %s" start_p.pos_fname
           start_p.pos_lnum col
           (Sedlexing.Utf8.lexeme buf))
  | _ -> failwith "unexpected end of input"

and read_string buf lexbuf =
  match%sedlex lexbuf with
  | '"' -> STRING (Buffer.contents buf)
  | Plus (Compl ('"' | '\\')) ->
      Buffer.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
      read_string buf lexbuf
  | '\\', '/' ->
      Buffer.add_char buf '/';
      read_string buf lexbuf
  | '\\', 't' ->
      Buffer.add_char buf '\t';
      read_string buf lexbuf
  | '\\', 'r' ->
      Buffer.add_char buf '\r';
      read_string buf lexbuf
  | '\\', 'n' ->
      Buffer.add_char buf '\n';
      read_string buf lexbuf
  | '\\', 'f' ->
      Buffer.add_char buf '\012';
      read_string buf lexbuf
  | '\\', 'b' ->
      Buffer.add_char buf '\b';
      read_string buf lexbuf
  | '\\', '\\' ->
      Buffer.add_char buf '\\';
      read_string buf lexbuf
  | eof -> raise @@ Errors.Error "unterminated string"
  | _ ->
      raise
      @@ Errors.Error
           ("illegal character in string: " ^ Sedlexing.Utf8.lexeme lexbuf)

let tokenize buf =
  let lbuf = Lexing.from_string "" in
  let tokenize lbuf =
    let tok = token buf in
    let start_p, end_p = Sedlexing.lexing_positions buf in
    lbuf.Lexing.lex_start_p <- start_p;
    lbuf.Lexing.lex_curr_p <- end_p;
    tok
  in
  (tokenize, lbuf)
