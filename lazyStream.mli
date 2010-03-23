(* Kaspar Rohrer, Tue Mar 23 06:47:43 CET 2010 *)

(** Lazy streams with pattern matching*)

exception Empty

type 'a cell =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

val empty : 'a t
val is_empty : 'a t -> bool

val cons : 'a -> 'a t -> 'a t

val slazy : (unit -> 'a cell) -> 'a t
val force : 'a t -> 'a cell

val hd : 'a t -> 'a
val tl : 'a t -> 'a t
