(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI Printing *)

(** Low-level ANSI printing *)

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

(** Attributes *)
module Attributes :
sig
  type t = attributes

  val make :
    ?intensity:intensity ->
    ?underline:underline ->
    ?inverted:bool ->
    ?blink:bool ->
    ?foreground:color ->
    ?background:color ->
    unit -> t

  val intensity  : t -> intensity
  val underline  : t -> underline
  val inverted   : t -> bool
  val blink      : t -> bool
  val foreground : t -> color
  val background : t -> color

  val set_intensity  : intensity -> t -> t
  val set_underline  : underline -> t -> t
  val set_inverted   : bool -> t -> t
  val set_blink      : bool -> t -> t
  val set_foreground : color -> t -> t
  val set_background : color -> t -> t
end

(** High-level streams and printing *)
module Text :
sig
  type size = int
  type line

  type raw =
    | RFrag of string
    | RAttr of attributes
    | RBreak
    | RLineBreak

  type cooked =
    | CFrag of string
    | CAttr of attributes
    | CSpace of int
    | CSeq of cooked array

  val empty_line : line
  val make_line : cooked array -> line
  val line_width : line -> size
  val line_concat : line list -> line

  val width_of_first_line : line LazyStream.t -> size
  val max_width_over_all_lines : line LazyStream.t -> size

  val format :
    ?attr:attributes ->
    ?fill:attributes ->
    ?width:size ->
    ?just:justification ->
    raw LazyStream.t -> line LazyStream.t

  val tabulate :
    ?attr:attributes ->
    ?fill:attributes ->
    line LazyStream.t list -> line LazyStream.t

  val pad :
    ?fill:attributes ->
    ?left:size ->
    ?right:size ->
    ?top:size ->
    ?bottom:size ->
    line LazyStream.t -> line LazyStream.t

  val indent :
    ?fill:attributes ->
    size ->
    line LazyStream.t -> line LazyStream.t

  val dump_raw : out_channel -> raw LazyStream.t -> unit

  val dump : out_channel -> line LazyStream.t -> unit
  val print : formatter -> line LazyStream.t -> unit
end
