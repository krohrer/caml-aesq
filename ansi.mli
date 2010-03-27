(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI Printing *)

(** { 6 Low-level ANSI printing } *)

type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justification = [`left | `center | `right | `block ]
type underline = [`single | `none]

type context
type ansi

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

(* TODO : these should operate on ansi, not on context. Provide
   context_* variants instead *)
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

val make : ?context:context -> out_channel -> ansi
val flush : ansi -> unit -> unit

val context : ansi -> context
val set_context : ansi -> context -> unit
val map_context : ansi -> (context -> context) -> unit

val print_space : ansi -> int -> unit
val print_string : ansi -> string -> unit
val print_newline : ansi -> unit -> unit
val printf : ansi -> ('a,unit,string,unit) format4 -> 'a

(** {6 High-level ANSI printing } *)
type 'a cell =
  | SNil
  | SCons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

type raw = [ `fragment of string | `break | `linebreak | `set_context of context ]
type linel = [ `fragment of string | `space of int | `set_context of context]

val append : 'a stream -> 'a stream -> 'a stream
val flatten : 'a stream list -> 'a stream

val format :
  ?width:int ->
  ?justification:justification ->
  ?context:context ->
  [< raw] stream -> [> linel] array stream

val dump_raw : out_channel -> [< raw] stream -> unit
val dump : out_channel -> [< linel] array stream -> unit

val print : ansi -> [< linel] array stream -> unit
