include Abs_types

let rec show e =
  match e with
  | Uni i ->
      "#" ^ string_of_int i
  | Var (s, n) ->
      s ^ "@" ^ string_of_int n
  | App (e0, e1) ->
      show e0 ^ " " ^ show e1
  | Pi (s, e0, e1) | Lam (s, e0, e1) ->
      "λ(" ^ s ^ " : " ^ show e0 ^ ")" ^ " -> " ^ show e1
