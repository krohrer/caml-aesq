(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI Printing *)

(** { 6 Low-level ANSI printing } *)

type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justification = [`left | `center | `right | `block ]
type underline = [`single | `none]

type context
type t

val make_context : 
  ?intensity:intensity ->
  ?underline:underline ->
  ?inverted:bool ->
  ?foreground:color ->
  ?background:color ->
  unit -> context

val color_to_string : color -> string
val intensity_to_string : intensity -> string
val justification_to_string : justification -> string
val underline_to_string : underline -> string
val context_to_string : context -> string

val intensity  : context -> intensity
val underline  : context -> underline
val inverted   : context -> bool
val foreground : context -> color
val background : context -> color

val set_intensity  : intensity -> context -> context
val set_underline  : underline -> context -> context
val set_inverted   : bool -> context -> context
val set_foreground : color -> context -> context
val set_background : color -> context -> context

val make : ?context:context -> out_channel -> t
val flush : t -> unit -> unit

val context : t -> context
val set_context : t -> context -> unit
val map_context : t -> (context -> context) -> unit

val print_space : t -> int -> unit
val print_string : t -> string -> unit
val print_newline : t -> unit -> unit
val printf : t -> ('a,unit,string,unit) format4 -> 'a

(** {6 High-level ANSI printing } *)

type 'a stream_cell =
  | SNil
  | SCons of 'a * 'a stream
and 'a stream = 'a stream_cell Lazy.t

val sappend : 'a stream -> 'a stream -> 'a stream
val sflatten : 'a stream list -> 'a stream

type ops = [ 
| `nop
| `set_intensity of intensity
| `set_underline of underline
| `set_inverted of bool
| `set_foreground of color
| `set_background of color
| `set_context of context
]

type format_ops = [
| `push_justification of justification
| `push_context of context
| `push_width of int
| `pop_justification
| `pop_context
| `pop_width
]

type frag = [ `fragment of string ]
type breaks = [ `break | `linebreak ]
type whitespaces = [ `space of int | `newline ]

(** Split text into lines for further processing*)
module LineSplitter :
sig
  type input  = [ frag | breaks | ops ]
  type output = [ frag | breaks | ops ]

  val split : width:int -> input stream -> [> output] stream
    (** Insert linebreaks so that output is no more than [width] columns. *)
end

(** Justify *)
module Justification :
sig
  type input  = [ frag | breaks | ops ]
  type output = [ frag | whitespaces | ops ]

  val justify : width:int -> justification -> input stream -> [> output] stream
    (** Justify linebroken text by converting breaks to spaces. *)
end

(**  *)
module Format :
sig
  type input  = [ `fragment of string | `break | `linebreak | `context of context ]
  type output = [ `fragment of string | `space of int | `context of context ]

  val format :
    ?width:int ->
    ?justification:justification ->
    ?context:context ->
    input stream -> [> output] array stream
end

(** Debug output of streams *)
module Debug :
sig
  type input = [ frag | whitespaces | breaks | ops | format_ops ]

  val dump : out_channel -> input stream -> unit
end

(** Pretty printing streams using ANSI codes *)
module Printer :
sig
  type input = [ frag | whitespaces | breaks | ops ]

  val print : t -> input stream -> unit
    (** Print stream *)
end
