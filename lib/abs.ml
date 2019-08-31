type variable = string * int
(** Variable with de Bruijn index *)

type universe = int
(** Universal level *)

(** Syntax tree for expressions *)
type expr =
  | Uni of universe
  | Var of variable
  | App of expr * expr
  | Lam of string * expr * expr
  | Pi of string * expr * expr
