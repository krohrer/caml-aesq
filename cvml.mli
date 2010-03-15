(* Kaspar Rohrer, Sun Mar 14 17:54:19 CET 2010 *)

(*----------------------------------------------------------------------------*)

type 'a t

type date
type text

val document : title -> 'a t -> unit t

val text : string -> text t
val 
