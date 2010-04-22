(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

type t

type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justification = [`left | `center | `right | `block | `none]
type underline = [`single | `none]

val color_to_string : color -> string
val intensity_to_string : intensity -> string
val justification_to_string : justification -> string
val underline_to_string : underline -> string
val attributes_to_string : attributes -> string

val make :
  ?intensity:intensity ->
  ?underline:underline ->
  ?inverted:bool ->
  ?blink:bool ->
  ?foreground:color ->
  ?background:color ->
  unit -> t

val default : t

type code = int
val code_of_intensity  : intensity -> code
val code_of_underline  : underline -> code
val code_of_inverted   : bool -> code
val code_of_blink      : bool -> code
val code_of_foreground : color -> code
val code_of_background : color -> code
val to_codes : t -> code list
val difference_to_codes : t -> t -> code list
val string_of_codes : code list -> string

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
