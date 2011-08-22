(* Kaspar Rohrer, Thu Apr 22 17:08:01 CEST 2010 *)

type color     = [ `black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default ]
let all_colors = [|`black ; `red ; `green ; `yellow ; `blue ; `magenta ; `cyan ; `white ; `default|]

type intensity      = [ `faint | `normal | `bold ]
let all_intensities = [|`faint ; `normal ; `bold|] 

type decoration     = [ `underline | `none ]
let all_decorations = [|`underline ; `none|]

type t = {
  intensity : intensity;
  decoration : decoration;
  inverted : bool;
  blink : bool;
  foreground : color;
  background : color;
}

type sequence = string

let intensity_to_string =
  function
    | `faint -> "faint"
    | `normal -> "normal"
    | `bold -> "bold"

let decoration_to_string =
  function
    | `underline -> "underline"
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

let to_string c =
  Printf.sprintf "{I=%s,U=%s,N=%b,B=%b,Fg=%s,Bg=%s}" 
    (intensity_to_string c.intensity)
    (decoration_to_string c.decoration)
    c.inverted
    c.blink
    (color_to_string c.foreground)
    (color_to_string c.background)

let make
    ?(intensity=`normal)
    ?(decoration=`none)
    ?(inverted=false)
    ?(blink=false)
    ?(foreground=`default)
    ?(background=`default)
    () = {
      intensity = intensity;
      decoration = decoration;
      inverted = inverted;
      blink = blink;
      foreground = foreground;
      background = background;
    }

let default = make ()

type code = int

let code_of_intensity =
  function
    | `bold -> 1
    | `normal -> 22
    | `faint -> 2

let code_of_decoration =
  function
    | `underline -> 4 
    | `none -> 24

let code_of_inverted =
  function
    | true -> 7
    | false -> 27

let code_of_blink =
  function
    | true -> 5
    | false -> 25

let color_code base c =
  let code = match c with
    | `black   -> 0
    | `red     -> 1
    | `green   -> 2 
    | `yellow  -> 3
    | `blue    -> 4
    | `magenta -> 5
    | `cyan    -> 6
    | `white   -> 7
    | `default -> 9
  in
    code + base

let code_of_foreground c =
  color_code 30 c

let code_of_background c =
  color_code 40 c

let to_codes a =
  code_of_intensity a.intensity
  :: code_of_decoration a.decoration
  :: code_of_inverted a.inverted
  :: code_of_blink a.blink
  :: code_of_foreground a.foreground
  :: code_of_background a.background
  :: []

let codes_of_transition a b =
  let codes = ref [] in
  let aux c a b =
    if a <> b then
      codes := c b :: !codes
    else
      ()
  in
    aux code_of_intensity a.intensity b.intensity;
    aux code_of_decoration a.decoration b.decoration;
    aux code_of_inverted a.inverted b.inverted;
    aux code_of_blink a.blink b.blink;
    aux code_of_foreground a.foreground b.foreground;
    aux code_of_background a.background b.background;
    !codes

let sequence_of_codes codes =
  let buf = Buffer.create 20 in
    (
      match codes with
	| [] ->
	    ()
	| [c] ->
	    Printf.bprintf buf "\x1b[%dm" c
	| c::rest ->
	    Printf.bprintf buf "\x1b[%d" c;
	    List.iter (fun c -> Printf.bprintf buf ";%d" c) rest;
	    Buffer.add_string buf "m"
    );
    Buffer.contents buf

let reset_sequence = "\x1b[0m"

let intensity a  = a.intensity
let decoration a = a.decoration
let inverted a   = a.inverted
let blink a      = a.blink
let foreground a = a.foreground
let background a = a.background

let set_intensity i a  = { a with intensity = i }
let set_decoration u a = { a with decoration = u }
let set_inverted i a   = { a with inverted = i }
let set_blink b a      = { a with blink = b }
let set_foreground c a = { a with foreground = c }
let set_background c a = { a with background = c }
