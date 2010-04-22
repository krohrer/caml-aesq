(* Kaspar Rohrer, Thu Apr 22 17:08:01 CEST 2010 *)

type t = {
  a_intensity : intensity;
  a_underline : underline;
  a_inverted : bool;
  a_blink : bool;
  a_foreground : color;
  a_background : color;
}

and color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
and intensity = [`faint | `normal | `bold]
and justification = [ `none | `left | `center | `right | `block]
and underline = [`single | `none]

let justification_to_string =
  function
    | `none -> "none"
    | `left -> "left"
    | `center -> "center"
    | `right -> "right"
    | `block -> "block"

let intensity_to_string =
  function
    | `faint -> "faint"
    | `normal -> "normal"
    | `bold -> "bold"

let underline_to_string =
  function
    | `single -> "single"
    | `none -> "none"

let color_to_string =
  function
    | `black -> "black"
    | `red -> "red"
    | `green -> "green"
    | `yellow -> "yellow"
    | `blue -> "blue"
    | `magenta -> "magenta"
    | `cyan -> "cyan"
    | `white -> "white"
    | `default -> "default"

let attributes_to_string c =
  Printf.sprintf "{I=%s,U=%s,N=%b,B=%b,Fg=%s,Bg=%s}" 
    (intensity_to_string c.a_intensity)
    (underline_to_string c.a_underline)
    c.a_inverted
    c.a_blink
    (color_to_string c.a_foreground)
    (color_to_string c.a_background)

let make
    ?(intensity=`normal)
    ?(underline=`none)
    ?(inverted=false)
    ?(blink=false)
    ?(foreground=`default)
    ?(background=`default)
    () = {
      a_intensity = intensity;
      a_underline = underline;
      a_inverted = inverted;
      a_blink = blink;
      a_foreground = foreground;
      a_background = background;
    }

let default = make ()

let intensity attrs  = attrs.a_intensity
let underline attrs  = attrs.a_underline
let inverted attrs   = attrs.a_inverted
let blink attrs      = attrs.a_blink
let foreground attrs = attrs.a_foreground
let background attrs = attrs.a_background

let set_intensity i attrs  = { attrs with a_intensity = i }
let set_underline u attrs  = { attrs with a_underline = u }
let set_inverted i attrs   = { attrs with a_inverted = i }
let set_blink b attrs      = { attrs with a_blink = b }
let set_foreground c attrs = { attrs with a_foreground = c }
let set_background c attrs = { attrs with a_background = c }
