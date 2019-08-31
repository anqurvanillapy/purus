{
open Lexing
open Parser

exception Syntax_error of string

let next_line buf =
  let pos = buf.lex_curr_p in
  buf.lex_curr_p <-
    { pos with pos_bol  = buf.lex_curr_pos
    ;          pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let ident = alpha (digit | alpha | '\'' | '_')*
let number = ('0' | ['1'-'9'] digit*)

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | ident as lxm { IDENT(lxm) }
  | number as lxm { NUMBER(int_of_string lxm) }
  | ':' { COLON }
  | "->" { RIGHT_ARROW }
  | '#' { UNIVERSE }
  | '\\' { LAMBDA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { DOT }
  | eof { EOF }
  | _ { raise (Syntax_error ("unexpected character: " ^ Lexing.lexeme lexbuf))
      }
