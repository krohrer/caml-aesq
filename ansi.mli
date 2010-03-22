(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI Printing *)

(** { 6 Low-level ANSI printing } *)

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

(** { 6 High-level ANSI printing *)
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
  type input = [ `linebreak | `break | `fragment of string | `ops of op list] Stream.t
  type output = int * input

  val separate_lines : width:int -> input -> output
    (** Insert linebreaks so that output is no more than [width] columns. *)
end

module Justification :
sig
  type input = LineSeparation.output
  type output = [ `fragment of string | `space of int | `ops of op list | `linebreak ] Stream.t

  val justify : justification -> input -> output
    (** Justify linebroken text by converting breaks to spaces. *)
end

module Printer :
sig
  type input = [ `fragment of string | `space of int | `ops of op list | `linebreak ] Stream.t

  val print : t -> input -> unit
    (** Print stream *)
end
