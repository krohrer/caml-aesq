(* Kaspar Rohrer, Sat Apr  3 13:54:47 CEST 2010 *)

type 'a t

val make : unit -> 'a t
val copy : 'a t -> 'a t

val replace : 'a t -> string -> 'a -> unit
val find : 'a t -> string -> 'a
