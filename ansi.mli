(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI Printing *)

type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justification = [`left | `center | `right | `block ]
type underline = [`single | `none]

type context
type t

val make_context : unit -> context
val default_context : t -> context

val make : out_channel -> t
val reset : t -> unit -> unit
val flush : t -> unit -> unit

val print_space : t -> int -> unit
val print_string : t -> string -> unit
val print_newline : t -> unit -> unit
val printf : t -> ('a,unit,string,unit) format4 -> 'a

val set_context : t -> context -> unit
val set_intensity  : t -> intensity -> unit
val set_underline  : t -> underline -> unit
val set_inverted   : t -> bool -> unit
val set_foreground : t -> color -> unit
val set_background : t -> color -> unit

type 'a stream_t = 
  | SEmpty
  | SCons of 'a * 'a stream_t lazy_t 

type op = [ 
| `set_intensity of intensity
| `set_underline of underline
| `set_inverted of bool
| `set_foreground of color
| `set_background of color
| `set_context of context
]

module LineSeparation :
sig
  type instream = [ `break | `fragment of string | `ops of op list] stream_t
  type outstream = [ `linebreak | `break | `fragment of string | `ops of op list] stream_t

  val separate_lines : width:int -> instream -> outstream
end

module Justification :
sig
  type instream = LineSeparation.outstream
  type outstream = [ `fragment of string | `ops of op list | `linebreak ] stream_t

  val justify : justification -> instream -> outstream
end

module Printer :
sig
  type instream = Justification.outstream

  val print : t -> instream -> unit
end
