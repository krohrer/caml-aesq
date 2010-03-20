type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type underline = [`single | `none]

type t = {
  p_outc : out_channel;
  mutable p_intensity : intensity;
  mutable p_underline : underline;
  mutable p_inverted : bool;
  mutable p_foreground : color;
  mutable p_background : color;
  mutable p_active_intensity : intensity;
  mutable p_active_underline : underline;
  mutable p_active_inverted : bool;
  mutable p_active_foreground : color;
mutable p_active_background : color
}


let make outc = {
  p_outc = outc;

  p_intensity = `normal;
  p_underline = `none;
  p_inverted = false;
  p_foreground = `default;
  p_background = `default;

  p_active_intensity = `normal;
  p_active_underline = `none;
  p_active_inverted = false;
  p_active_foreground = `default;
  p_active_background = `default;
}

let reset p () =
  p.p_intensity <- `normal;
  p.p_underline <- `none;
  p.p_inverted <- false;
  p.p_foreground <- `default;
  p.p_background <- `default

let set_intensity p i =
  p.p_intensity <- i
  
let set_underline p u = 
  p.p_underline <- u

let set_inverted p i =
  p.p_inverted <- i

let set_foreground p c =
  p.p_foreground <- c

let set_background p c =
  p.p_background <- c

let enforce_intensity p codes =
  let i = p.p_intensity in
    if i <> p.p_active_intensity then begin
      p.p_active_intensity <- i;
      let c =
	match i with
	  | `bold -> 1
	  | `normal -> 22
	  | `faint -> 2
      in
	c :: codes
    end else
      codes

let enforce_underline p codes =
  let u = p.p_underline in
    if u <> p.p_active_underline then begin
      p.p_active_underline <- u;
      let c =
	match u with
	  | `single -> 4 
	  | `none -> 24
      in
	c :: codes
    end else
      codes

let enforce_inverted p codes =
  let i = p.p_inverted in
    if i <> p.p_active_inverted then begin
      p.p_active_inverted <- i;
      let c = 
	match i with
	  | true -> 7
	  | false -> 27
      in
	c :: codes
    end else
      codes

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

let enforce_foreground p codes =
  let c = p.p_foreground in
    if c <> p.p_active_foreground then begin
      p.p_active_foreground <- c;
      (color_code 30 c) :: codes
    end else
      codes

let enforce_background p codes =
  let c = p.p_background in
    if c <> p.p_active_background then begin
      p.p_active_background <- c;
      (color_code 40 c) :: codes
    end else
      codes

let print_string p s =
  output_string p.p_outc s

let print_int p i =
  output_string p.p_outc (string_of_int i)

let print_ansi_codes p =
  function
    | [] -> ()
    | [c] -> print_string p "\x1b["; print_int p c; print_string p "m"
    | c::rest ->
	print_string p "\x1b[";
	print_int p c;
	List.iter (fun c -> print_string p ";"; print_int p c) rest;
	print_string p "m"

let enforce_attributes p () =
  print_ansi_codes p
    (enforce_intensity p
      (enforce_underline p
	 (enforce_inverted p
	    (enforce_background p
	       (enforce_foreground p [])))))

let print_space p n =
  enforce_attributes p ();
  for i = 1 to n do
    print_string p " "
  done

let print_string p s =
  enforce_attributes p ();
  print_string p s

let print_newline p () =
  print_string p "\n"

let printf p fmt =
  enforce_attributes p ();
  Printf.ksprintf (print_string p) fmt
    
let flush p () =
  enforce_attributes p ();
  Pervasives.flush p.p_outc
