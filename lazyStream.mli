(* Kaspar Rohrer, Tue Mar 23 06:47:43 CET 2010 *)

(** Lazy streams with pattern matching*)

exception Empty

type 'a t =
  | Nil
  | Cons of 'a * 'a t Lazy.t

val empty : 'a t
val is_empty : 'a t -> bool

val cons : 'a -> 'a t Lazy.t -> 'a t
val cons2 : 'a -> 'a -> 'a t Lazy.t -> 'a t
val lcons : 'a -> (unit -> 'a t) -> 'a t

val hd : 'a t -> 'a
val tl : 'a t -> 'a t
