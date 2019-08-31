module Ctx = Ctx
module Abs = Abs
module Typecheck = Typecheck

let run () =
  let lexbuf = Lexing.from_channel stdin in
  match Parser.top Lexer.read lexbuf with
  | Some e ->
      print_endline (Abs.show (Typecheck.type_of e)) ;
      print_endline "Finished checking."
  | None ->
      exit 0
