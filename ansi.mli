(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI Printing *)

(** { 6 Low-level ANSI printing } *)

type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justification = [`left | `center | `right | `block | `none]
type underline = [`single | `none]

type attributes
type formatter

val color_to_string : color -> string
val intensity_to_string : intensity -> string
val justification_to_string : justification -> string
val underline_to_string : underline -> string
val attributes_to_string : attributes -> string

val default_attributes : attributes
val std_formatter : formatter

val make_formatter : ?attributes:attributes -> out_channel -> formatter

val attributes : formatter -> attributes
val set_attributes : formatter -> attributes -> unit
val map_attributes : formatter -> (attributes -> attributes) -> unit

val print_space : formatter -> int -> unit
val print_string : formatter -> string -> unit
val print_newline : formatter -> unit -> unit
val printf : formatter -> ('a,unit,string,unit) format4 -> 'a

val flush : formatter -> unit -> unit

(** {6 Attributes} *)
module Attributes :
sig
  type t = attributes

  val make :
    ?intensity:intensity ->
    ?underline:underline ->
    ?inverted:bool ->
    ?foreground:color ->
    ?background:color ->
    unit -> t

  val intensity  : t -> intensity
  val underline  : t -> underline
  val inverted   : t -> bool
  val foreground : t -> color
  val background : t -> color

  val set_intensity  : intensity -> t -> t
  val set_underline  : underline -> t -> t
  val set_inverted   : bool -> t -> t
  val set_foreground : color -> t -> t
  val set_background : color -> t -> t
end

(** {6 High-level streams and printing} *)
module Text :
sig
  type printable = [ `fragment of string | `space of int ]

  type width = int
  type line

  type raw =
      [ `fragment of string
      | `attributes of attributes
      | `break
      | `linebreak
      ]

  type cooked =
      [ `fragment of string
      | `attributes of attributes
      | `space of int
      | `seq of cooked array
      ]

  val empty_line : line
  val make_line : cooked array -> line
  val line_width : line -> width
  val line_concat : line list -> line

  val format :
    ?attributes:attributes -> ?width:width -> ?justification:justification ->
    raw LazyStream.t -> line LazyStream.t

  val tabulate :
    ?separator:cooked -> widths:width list ->
    line LazyStream.t list -> line LazyStream.t

  val dump_raw : out_channel -> raw LazyStream.t -> unit

  val dump : out_channel -> line LazyStream.t -> unit
  val print : formatter -> line LazyStream.t -> unit
end
