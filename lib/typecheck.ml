open Abs
module Ctx = Ctx

exception Unbound_variable of string

exception Invalid_input_type of string

exception Invalid_output_type of string

exception Typecheck_error of string

let axiom u = u + 1

let rule e0 e1 =
  match (e0, e1) with
  | Uni u0, Uni u1 ->
      Uni (max u0 u1)
  | _ ->
      raise (Invalid_argument "expr")

let shift d x0 e0 =
  let rec go e c =
    match e with
    | Lam (x, _A, b) ->
        let c' = if x = x0 then c + 1 else c in
        Lam (x, go _A c, go b c')
    | Pi (x, _A, _B) ->
        let c' = if x = x0 then c + 1 else c in
        Pi (x, go _A c, go _B c')
    | App (f, a) ->
        App (go f c, go a c)
    | Var (x, n) ->
        let n' = if x = x0 && n >= c then n + d else n in
        Var (x, n')
    | Uni l ->
        Uni l
  in
  go e0 0

let rec subst x n e' e =
  match e with
  | Lam (x', _A, b) ->
      let n' = if x = x' then n + 1 else n in
      let b' = subst x n' (shift 1 x' e') b in
      Lam (x', subst x n e' _A, b')
  | Pi (x', _A, _B) ->
      let n' = if x = x' then n + 1 else n in
      let _B' = subst x n' (shift 1 x' e') _B in
      Pi (x', subst x n e' _A, _B')
  | App (f, a) ->
      App (subst x n e' f, subst x n e' a)
  | Var (x', n') ->
      if x = x' && n = n' then e' else e
  | Uni l ->
      Uni l

let rec whnf e =
  match e with
  | App (f, a) -> (
    match whnf f with
    | Lam (x, _A, b) ->
        let a' = shift 1 x a in
        let b' = subst x 0 a' b in
        whnf (shift (-1) x b')
    | f' ->
        App (f', a) )
  | _ ->
      e

let rec is_free (x, n) ex =
  let rec go e =
    match e with
    | Lam (x', _A, b) ->
        let n' = n + 1 in
        go _A || if x = x' then is_free (x, n') b else go b
    | Pi (x', _A, _B) ->
        let n' = n + 1 in
        go _A || if x = x' then is_free (x, n') _B else go _B
    | Var (x', n') ->
        x = x' && n = n'
    | App (f, a) ->
        go f || go a
    | _ ->
        false
  in
  go ex

let rec normalize e =
  match e with
  | Lam (x, _A, b) -> (
      let b' = normalize b in
      let e' = Lam (x, normalize _A, b') in
      match b' with
      | App (f, a) -> (
          let a' = whnf a in
          match a' with
          | Var (x', n') when x = x' && 0 = n' && not (is_free (x', n') f) ->
              (* Eta-reduction *)
              shift (-1) x f
          | _ ->
              e' )
      | _ ->
          e' )
  | Pi (x, _A, _B) ->
      Pi (x, normalize _A, normalize _B)
  | App (f, a) -> (
    match normalize f with
    (* Beta-reduction *)
    | Lam (x, _A, b) ->
        let a' = shift 1 x (normalize a) in
        let b' = subst x 0 a' b in
        normalize (shift (-1) x b')
    | f' ->
        App (f', normalize a) )
  | Var _ ->
      e
  | Uni _ ->
      e

let rec type_with (ctx : expr Ctx.ctx) (e : expr) : expr =
  match e with
  | Uni u ->
      Uni (axiom u)
  | Var (x, n) -> (
    match Ctx.lookup x n ctx with
    | None ->
        Ctx.print ctx ; raise (Unbound_variable x)
    | Some a ->
        a )
  | Lam (x, _A, b) ->
      let _ = type_with ctx _A in
      let ctx' = Ctx.map (fun (s, e) -> (s, shift 1 x e)) (Ctx.add x _A ctx) in
      let _B = type_with ctx' b in
      let p = Pi (x, _A, _B) in
      let _ = type_with ctx p in
      p
  | Pi (x, _A, _B) ->
      let eS = whnf (type_with ctx _A) in
      let s =
        match eS with Uni n -> Uni n | _ -> raise (Invalid_input_type x)
      in
      let ctx' = Ctx.map (fun (s, e) -> (s, shift 1 x e)) (Ctx.add x _A ctx) in
      let eT = whnf (type_with ctx' _B) in
      let t =
        match eT with Uni n -> Uni n | _ -> raise (Invalid_output_type x)
      in
      rule s t
  | App (f, a) ->
      let e' = whnf (type_with ctx f) in
      let x, _A, _B =
        match e' with
        | Pi (x, _A, _B) ->
            (x, _A, _B)
        | _ ->
            raise (Typecheck_error "not a function")
      in
      let _A' = type_with ctx a in
      if _A = _A' then
        let a' = shift 1 x a in
        let _B' = subst x 0 a' _B in
        shift (-1) x _B'
      else raise (Typecheck_error "type mismatch")

let type_of = type_with (Ctx.empty ())
