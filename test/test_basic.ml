open Purus
open Abs
open Typecheck

let test_basic () =
  let u0 = Uni 0 in
  let _ = type_of u0 in
  let id = Lam ("a", Uni 0, Lam ("x", Var ("a", 0), Var ("x", 0))) in
  let _ = type_of id in
  ()
