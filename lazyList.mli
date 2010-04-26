(* Kaspar Rohrer, Tue Mar 23 06:47:43 CET 2010 *)

(** Lazy lists *)

exception Empty

type 'a cell =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

val cell : 'a -> 'a t -> 'a cell
val nil : 'a t
val cons : 'a -> 'a t -> 'a t
val sing : 'a -> 'a t

val generate : (unit -> 'a) -> 'a t 
val forever : 'a -> 'a t
val from : (unit -> 'a option) -> 'a t

val hd : 'a t -> 'a
val tl : 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val iter : ('a -> unit) -> 'a t -> unit

val append : 'a t -> 'a t -> 'a t
val flatten : 'a t list -> 'a t

val length : 'a t -> int
val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t

val is_nil : 'a t -> bool
val is_cons : 'a t -> bool
