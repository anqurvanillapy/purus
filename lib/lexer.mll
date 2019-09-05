{
open Lexing
open Parser

exception Syntax_error of string

let last_token = ref EOF

let depth = ref 0

let next_line buf =
  let pos = buf.lex_curr_p in
  buf.lex_curr_p <-
    { pos with pos_bol  = buf.lex_curr_pos
    ;          pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t' '\r']+
let newline = '\n'
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let ident = alpha (digit | alpha | '\'' | '_')*
let number = ('0' | ['1'-'9'] digit*)

rule read =
  parse
  | white { read lexbuf }
  | newline
  {
    next_line lexbuf;
    match !depth, !last_token with
    | (0, IDENT(_)) | (0, RPAREN) ->
      last_token := SEMICOLON; !last_token
    | _ ->
      read lexbuf
  }
  | ident as lxm { last_token := IDENT(lxm); !last_token }
  | number as lxm { last_token := NUMBER(int_of_string lxm); !last_token }
  | ':' { last_token := COLON; !last_token }
  | "->" { last_token := RIGHT_ARROW; !last_token }
  | '#' { last_token := UNIVERSE; !last_token }
  | '\\' { last_token := LAMBDA; !last_token }
  | '(' { depth := !depth + 1; last_token := LPAREN; !last_token }
  | ')' { depth := !depth - 1; last_token := RPAREN; !last_token }
  | ';' { last_token := SEMICOLON; !last_token }
  | "(*" { comment lexbuf; read lexbuf }
  | eof { last_token := EOF; !last_token }
  | _ { raise (Syntax_error ("unexpected character: " ^ Lexing.lexeme lexbuf))
      }

and comment =
  parse
  | "*)" {}
  | _ { comment lexbuf }
