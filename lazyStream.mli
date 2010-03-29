(* Kaspar Rohrer, Tue Mar 23 06:47:43 CET 2010 *)

(** Lazy streams with pattern matching*)

exception Empty

type 'a cell =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

val cell : 'a -> 'a t -> 'a cell
val cons : 'a -> 'a t -> 'a t
val sing : 'a -> 'a t

val hd : 'a t -> 'a
val tl : 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val append : 'a t -> 'a t -> 'a t
val flatten : 'a t list -> 'a t
