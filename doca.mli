(* Kaspar Rohrer, Mon Mar 15 01:05:15 CET 2010 *)

(** Doca: A simple document generator in OCaml *)

(** Document representation*)

type text = [`text]
type elem = [`elem|text]

type 't node

type document

val document :
  ?author:string ->
  ?header:(?page:int * int -> unit -> text node) ->
  ?footer:(?page:int * int -> unit -> text node) ->
  elem node list -> document

val text : string -> [>`text] node
val break : [>`text] node

val emph : text node -> [>`text] node
val verb : text node -> [>`text] node
val blink : text node -> [>`text] node
val code : text node -> [>`text] node
val left : ([<elem] as 'l) node -> 'l node

val section : text node -> elem node list -> [>`elem] node
val table : text node -> elem node list list -> [>`elem] node

val (~~) : string -> [>`text] node

(** Localization *)

type 'l lang

val language : 'l -> 'l lang
val localize : 'l lang -> ('l lang * 'a) list -> 'a

(** Printing *)

val ansi_print : ?offset:int -> ?width:int -> out_channel -> document -> unit
