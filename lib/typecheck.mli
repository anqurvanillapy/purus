open Abs

val axiom : universe -> universe
(** The Axiom for a universe *)

val rule : expr -> expr -> expr
(** The Rule for two universes *)

val shift : int -> string -> expr -> expr
(** `shift n x` adds `n` to the index of all free variables named `x` within an
    expression *)

val subst : string -> int -> expr -> expr -> expr
(** Substitute all occurrences of a variable in an expression *)

val whnf : expr -> expr
(** Reduce an expression to its weak-head normal form *)

val is_free : variable -> expr -> bool
(** Check if a variable is free in an expression *)

val normalize : expr -> expr
(** Reduce an expression to its normal form, performing both beta reduction and
    eta reduction *)

val type_with : expr Ctx.ctx -> expr -> expr
(** Type check an expression and return the expression's type *)

val type_of : expr -> expr
(** Type check with an empty context *)
