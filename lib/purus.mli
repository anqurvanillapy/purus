module Ctx = Ctx

val run : unit -> unit

type variable = string * int
(** Variable with de Bruijn index *)

type universe = int
(** Universal level *)

(** Syntax tree for expressions *)
type 'a expr =
  | Uni of universe
  | Var of variable
  | App of 'a expr * 'a expr
  | Lam of string * 'a expr * 'a expr
  | Pi of string * 'a expr * 'a expr

val axiom : universe -> universe
(** The Axiom for a universe *)

val rule : 'a expr -> 'a expr -> 'a expr
(** The Rule for two universes *)

val shift : int -> string -> 'a expr -> 'a expr
(** `shift n x` adds `n` to the index of all free variables named `x` within an
    expression *)

val subst : string -> int -> 'a expr -> 'a expr -> 'a expr
(** Substitute all occurrences of a variable in an expression *)

val whnf : 'a expr -> 'a expr
(** Reduce an expression to its weak-head normal form *)

val is_free : variable -> 'a expr -> bool
(** Check if a variable is free in an expression *)

val normalize : 'a expr -> 'a expr
(** Reduce an expression to its normal form, performing both beta reduction and
    eta reduction *)

val type_with : unit expr Ctx.ctx -> unit expr -> unit expr
(** Type check an expression and return the expression's type *)

val type_of : unit expr -> unit expr
(** Type check with an empty context *)
