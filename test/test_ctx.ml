open Purus

let test_ctx () =
  let ctx = Ctx.add "b" 22 (Ctx.add "a" 11 (Ctx.empty ())) in
  let _ = assert (Option.is_some (Ctx.lookup "a" 0 ctx)) in
  let ctx' = Ctx.add "b" 22 (Ctx.add "a" 11 (Ctx.empty ())) in
  let _ = assert (Option.is_none (Ctx.lookup "a" 1 ctx')) in
  let ctx'' =
    Ctx.add "a" 33 (Ctx.add "b" 22 (Ctx.add "a" 11 (Ctx.empty ())))
  in
  let _ = assert (Option.get (Ctx.lookup "a" 1 ctx'') == 11) in
  ()
