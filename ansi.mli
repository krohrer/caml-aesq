(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI Printing *)

type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justify = [`left | `center | `right | `block ]
type underline = [`single | `none]

type t

val make : out_channel -> t
val reset : t -> unit -> unit
val flush : t -> unit -> unit

val print_space : t -> int -> unit
val print_string : t -> string -> unit
val print_newline : t -> unit -> unit
val printf : t -> ('a,unit,string,unit) format4 -> 'a

val set_intensity  : t -> intensity -> unit
val set_underline  : t -> underline -> unit
val set_inverted   : t -> bool -> unit
val set_foreground : t -> color -> unit
val set_background : t -> color -> unit
