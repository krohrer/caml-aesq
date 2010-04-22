(* Kaspar Rohrer, Fri Apr 23 00:11:57 CEST 2010 *)

(** {6 Types} *)

type size = int
type line
type justification = [`left | `center | `right | `block | `none]

(** {6 Input} *)

type raw =
  | RFrag of string
  | RAttr of Ansi.t
  | RBreak
  | RLineBreak

(** {6 Output} *)

type cooked =
  | CFrag of string
  | CAttr of Ansi.t
  | CSpace of int
  | CSeq of cooked array

(** {6 Lines} *)

val empty_line : line
val make_line : cooked array -> line
val line_width : line -> size
val line_concat : line list -> line

val width_of_first_line : line LazyStream.t -> size
val max_width_over_all_lines : line LazyStream.t -> size

(** {6 Layout} *)

val format :
  ?attr:Ansi.t ->
  ?fill:Ansi.t ->
  ?width:size ->
  ?just:justification ->
  raw LazyStream.t -> line LazyStream.t

val tabulate :
  ?attr:Ansi.t ->
  ?fill:Ansi.t ->
  line LazyStream.t list -> line LazyStream.t

val pad :
  ?fill:Ansi.t ->
  ?left:size ->
  ?right:size ->
  ?top:size ->
  ?bottom:size ->
  line LazyStream.t -> line LazyStream.t

val indent :
  ?fill:Ansi.t ->
  size ->
  line LazyStream.t -> line LazyStream.t

val print : ?attr:Ansi.t -> out_channel -> line LazyStream.t -> unit

(**/**)

val justification_to_string : justification -> string

val dump_raw : out_channel -> raw LazyStream.t -> unit
val dump : out_channel -> line LazyStream.t -> unit

