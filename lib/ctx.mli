type 'a ctx
(** Typing context *)

val print : 'a ctx -> unit

val empty : unit -> 'a ctx
(** Returns an empty context *)

val add : string -> 'a -> 'a ctx -> 'a ctx
(** Add to a context *)

val lookup : string -> int -> 'a ctx -> 'a option
(** Lookup by name and index *)

val map : (string * 'a -> string * 'a) -> 'a ctx -> 'a ctx
