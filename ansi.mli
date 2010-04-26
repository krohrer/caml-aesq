(* Kaspar Rohrer, Sat Mar 20 16:31:31 CET 2010 *)

(** ANSI attributes / escape sequences *)

(** {6 Types} *)

type t
type sequence = string

type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type underline = [`single | `none]

val color_to_string : color -> string
val intensity_to_string : intensity -> string
val underline_to_string : underline -> string

val to_string : t -> string

val make :
  ?intensity:intensity ->
  ?underline:underline ->
  ?inverted:bool ->
  ?blink:bool ->
  ?foreground:color ->
  ?background:color ->
  unit -> t

val default : t

type code
val code_of_intensity  : intensity -> code
val code_of_underline  : underline -> code
val code_of_inverted   : bool -> code
val code_of_blink      : bool -> code
val code_of_foreground : color -> code
val code_of_background : color -> code
val to_codes : t -> code list
val codes_of_transition : t -> t -> code list
val sequence_of_codes : code list -> sequence
val reset_sequence : sequence

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
