module Ctx = Ctx
module Abs = Abs
module Typecheck = Typecheck

let infile_name = ref None

let specs = []

let usage = "usage: purus [FILE]"

let _ = Arg.parse specs (fun n -> infile_name := Some n) usage

let run () =
  let fname, ic =
    match !infile_name with
    | Some f ->
        (f, open_in_bin f)
    | None ->
        ("<stdin>", stdin)
  in
  let lexbuf = Lexing.from_channel ic in
  try
    match Parser.top Lexer.read lexbuf with
    | Some e ->
        print_endline (Abs.show (Typecheck.type_of e))
    | None ->
        exit 0
  with e ->
    let exn_name = Printexc.to_string e in
    let p = Lexing.lexeme_start_p lexbuf in
    Printf.fprintf stderr "%s:%d:%d: error: %s" fname p.Lexing.pos_lnum
      (p.Lexing.pos_cnum - p.Lexing.pos_bol)
      exn_name ;
    exit 1
