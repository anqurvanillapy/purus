module Parser = Parser
module Lexer = Lexer

let run () =
  let lexbuf = Lexing.from_channel stdin in
  match Parser.top Lexer.read lexbuf with
  | Some e ->
      let _ = Typecheck.type_of e in
      print_endline "Finished checking." ;
      flush stdout
  | None ->
      exit 0
