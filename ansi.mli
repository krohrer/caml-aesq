(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI attributes / escape sequences *)

(** {6 Types} *)

type t
type sequence = string

type color      = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity  = [`faint | `normal | `bold]
type decoration = [`underline | `none]

val all_colors      : color array
val all_intensities : intensity array
val all_decorations : decoration array

val color_to_string      : color -> string
val intensity_to_string  : intensity -> string
val decoration_to_string : decoration -> string

val to_string : t -> string

val make :
  ?intensity:intensity ->
  ?decoration:decoration ->
  ?inverted:bool ->
  ?blink:bool ->
  ?foreground:color ->
  ?background:color ->
  unit -> t

val default : t

type code
val code_of_intensity  : intensity -> code
val code_of_decoration : decoration -> code
val code_of_inverted   : bool -> code
val code_of_blink      : bool -> code
val code_of_foreground : color -> code
val code_of_background : color -> code
val to_codes : t -> code list
val codes_of_transition : t -> t -> code list
val sequence_of_codes : code list -> sequence
val reset_sequence : sequence

val intensity   : t -> intensity
val decoration  : t -> decoration
val inverted    : t -> bool
val blink       : t -> bool
val foreground  : t -> color
val background  : t -> color

val set_intensity   : intensity -> t -> t
val set_decoration  : decoration -> t -> t
val set_inverted    : bool -> t -> t
val set_blink       : bool -> t -> t
val set_foreground  : color -> t -> t
val set_background  : color -> t -> t
