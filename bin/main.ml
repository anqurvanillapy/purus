let fname = ref None

let _ = Arg.parse [] (fun f -> fname := Some f) "usage: purus [FILE]"

let () = Purus.run ~file:!fname
