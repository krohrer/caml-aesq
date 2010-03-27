type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justification = [`left | `center | `right | `block ]
type underline = [`single | `none]

let justification_to_string =
  function
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

type context = {
  c_intensity : intensity;
  c_underline : underline;
  c_inverted : bool;
  c_foreground : color;
  c_background : color;
}

let context_to_string c =
  Printf.sprintf "{intensity=%s, underline=%s, inverted=%b, foreground=%s, background=%s}" 
    (intensity_to_string c.c_intensity)
    (underline_to_string c.c_underline)
    c.c_inverted
    (color_to_string c.c_foreground)
    (color_to_string c.c_background)

type ansi = {
  mutable p_intensity : intensity;
  mutable p_underline : underline;
  mutable p_inverted : bool;
  mutable p_foreground : color;
  mutable p_background : color;
  p_outc : out_channel;
  mutable p_context : context;
}

let make_context 
  ?(intensity=`normal)
  ?(underline=`none)
  ?(inverted=false)
  ?(foreground=`default)
  ?(background=`default)
  () = {
    c_intensity = intensity;
    c_underline = underline;
    c_inverted = inverted;
    c_foreground = foreground;
    c_background = background;
  }

let make ?context outc =
  let context =
    match context with
      | None -> make_context ()
      | Some c -> c
  in
    {
      p_intensity = `normal;
      p_underline = `none;
      p_inverted = false;
      p_foreground = `default;
      p_background = `default;
      p_outc = outc;
      p_context = context;
    }

let context p =
  p.p_context

let set_context p ctx =
  p.p_context <- ctx

let map_context p f =
  p.p_context <- f p.p_context

let intensity ctx = ctx.c_intensity
let underline ctx = ctx.c_underline
let inverted ctx  = ctx.c_inverted
let foreground ctx = ctx.c_foreground
let background ctx = ctx.c_background

let set_intensity i ctx  = { ctx with c_intensity = i }
let set_underline u ctx  = { ctx with c_underline = u }
let set_inverted i ctx   = { ctx with c_inverted = i }
let set_foreground c ctx = { ctx with c_foreground = c }
let set_background c ctx = { ctx with c_background = c }

let enforce_intensity p codes =
  let i = p.p_context.c_intensity in
    if i <> p.p_intensity then begin
      p.p_intensity <- i;
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
  let u = p.p_context.c_underline in
    if u <> p.p_underline then begin
      p.p_underline <- u;
      let c =
        match u with
          | `single -> 4 
          | `none -> 24
      in
        c :: codes
    end else
      codes

let enforce_inverted p codes =
  let i = p.p_context.c_inverted in
    if i <> p.p_inverted then begin
      p.p_inverted <- i;
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
  let c = p.p_context.c_foreground in
    if c <> p.p_foreground then begin
      p.p_foreground <- c;
      (color_code 30 c) :: codes
    end else
      codes

let enforce_background p codes =
  let c = p.p_context.c_background in
    if c <> p.p_background then begin
      p.p_background <- c;
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

(*----------------------------------------------------------------------------*)

open ExtLib

type 'a cell =
  | SNil
  | SCons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

type raw = [ `fragment of string | `break | `linebreak | `set_context of context ]
type linel = [ `fragment of string | `space of int | `set_context of context]

let rec append s sapp =
  match Lazy.force s with
    | SNil -> sapp
    | SCons (x, s) ->
	lazy (SCons (x, append s sapp))

let flatten slist =
  let rec fold s srest =
    match Lazy.force s with
      | SNil ->
	  begin match srest with
	    | [] ->
		SNil
	    | s::srest ->
		fold s srest
	  end
      | SCons (x, s) ->
	  SCons (x, lazy (fold s srest))
  in
    match slist with
      | [] -> lazy SNil
      | s::srest -> lazy (fold s srest)

(* Fill the line-array backwards *)
let rec fill_line ~line ~break_space ~break_count ?(a=break_space) k =
  function
    | [] -> ()
    | `break::rest ->
	(* Bresenham stepping for greater accuracy! *)
	let a = a mod break_count + break_space in
	  line.(k) <- `space (a / break_count);
	  fill_line ~line ~break_space ~break_count ~a (k - 1) rest
    | (`fragment _ as x)::rest
    | (`set_context _ as x)::rest ->
	line.(k) <- x;
	fill_line ~line ~break_space ~break_count ~a (k - 1) rest

(* Measure line and possibly find last active context (for next line) *)
let rec measure_line ?(count=0) ?(break_count=0) ?(length=0) ?last_context =
  function
    | [] ->
	count, break_count, length, last_context
    | x::rest ->
	let count = count + 1 in
	  match x with
	    | `fragment f ->
		let length = length + String.length f in
		  measure_line ~count ~break_count ~length ?last_context rest
	    | `break ->
		let break_count = break_count + 1 in
		  measure_line ~count ~break_count ~length ?last_context rest
	    | `set_context c ->
		let last_context = Option.default c last_context in
		  measure_line ~count ~break_count ~length ~last_context rest

(* Return the context for the next line and an array of justified
   line elements *)
let justify_line ?(partial=false) ~width ~justification ~line_rev context =
  let count, break_count, length, last_context = measure_line line_rev in
  let justification =
    match justification with
      | `block -> if partial || break_count = 0 then `left else `right
      | j -> j
  in
  let line =
    match justification with
      | `left ->
	  (* Add left-over space on the right side *)
	  let space = width - length - break_count in
	  let break_space = break_count in
	  let line = Array.make (count + 2) (`set_context context) in
	    (* line.(0) <- `set_context context; *)
	    fill_line ~line ~break_space ~break_count count line_rev;
	    line.(count + 1) <- `space space;
	    line
      | `center ->
	  (* Add a bit of left-over space on both sides *)
	  let space = width - length - break_count in
	  let break_space = break_count in
	  let left_space = space / 2 in
	  let right_space = space - left_space in
	  let line = Array.make (count + 3) (`set_context context) in
	    (* line.(0) <- `set_context context; *)
	    line.(1) <- `space left_space;
	    fill_line ~line ~break_space ~break_count (count + 1) line_rev;
	    line.(count + 2) <- `space right_space;
	    line
      | `right ->
	  (* Add left-over space on the left side *)
	  let space = width - length - break_count in
	  let break_space = break_count in
	  let line = Array.make (count + 2) (`set_context context) in
	    (* line.(0) <- `set_context context; *)
	    line.(1) <- `space space;
	    fill_line ~line ~break_space ~break_count (count + 1) line_rev;
	    line
      | `block ->
	  (* Distribute *)
	  assert (break_count > 0);
	  let break_space = width - length in
	  let line = Array.make (count + 2) (`set_context context) in
	    (* line.(0) <- `set_context context; *)
	    fill_line ~line ~break_space ~break_count count line_rev;
	    line
  in
    Option.default context last_context, line

(* Format: Width and justification can be part of a closure, so we
   dont have to pass them along as parameters *)
let format
    ?(width=78)
    ?(justification=`block)
    =
  (* Collect line elements for justification *)
  let rec collect_line
      ?(rem_width=width) 
      ?(has_break=false) 
      ?(line_rev=[])
      context
      stream 
      =
    match Lazy.force stream with
      | SNil ->
	  let _, line =
	    justify_line ~partial:true ~width ~justification ~line_rev context
	  in
	  let sappend = lazy SNil in
	    SCons (line, sappend) 
      | SCons (x, stream) ->
	  match x with
	    | `fragment _ as f ->
		collect_fragment ~rem_width ~has_break ~line_rev f context stream
	    | `break ->
		let has_break = rem_width <> width in
		  collect_line ~rem_width ~has_break ~line_rev context stream
	    | `linebreak ->
		(* A linebreak *)
		let context, line = 
		  justify_line ~partial:true ~width ~justification ~line_rev context
		and sappend =
		  lazy (collect_line context stream)
		in
		  SCons (line, sappend)
	    | `set_context _ as x ->
		(* Passthrough for the moment. [context] is the
		   context at the start of the current line, so we
		   cannot yet change it to something else.*)
		let line_rev = x :: line_rev in
		  collect_line ~rem_width ~has_break ~line_rev context stream

  and collect_fragment
      ?(rem_width=width)
      ?(has_break=false)
      ?(line_rev=[])
      (`fragment frag as f)
      context
      stream
      =
    (* Collect fragment and justify line if necessary *)
    let len = String.length frag in
    let break_len =
      if has_break then
	len + 1
      else
	len
    in
      if break_len <= rem_width then
	(* Fragment (and possibly break) still fit on this line *)
	let rem_width = rem_width - break_len in
	let line_rev =
	  if has_break then
	    `break :: f :: line_rev
	  else
	    f :: line_rev
	in
	  collect_line ~rem_width ~line_rev context stream
      else if len > width then
	(* Fragment must be split anyway, may as well start on this
	   line *)
	let frag_left = String.slice ~last:rem_width frag and
	    frag_right = String.slice ~first:rem_width frag
	in
	let line_rev =
	  if has_break then
	    `break :: `fragment frag_left :: line_rev
	  else 
	    `fragment frag_left :: line_rev
	in
	let context, line = justify_line ~width ~justification ~line_rev context in
	let sappend = lazy (collect_fragment (`fragment frag_right) context stream) in
	  SCons (line, sappend)
      else
	(* Fragment fits on next line for sure *)
	let context, line = justify_line ~width ~justification ~line_rev context in
	let sappend = lazy (collect_line context stream)
	in
	  SCons (line, sappend)
  in
    (* We do not want the stream to be in the closure above.  It
     * would not be garbage collected otherwise.  *)
  let format' ?(context=make_context ()) stream =
    lazy (collect_line context stream)
  in
    format'

(*----------------------------------------------------------------------------*)

open Printf

let rec dump_raw outc stream =
  match Lazy.force stream with
    | SNil ->
	fprintf outc "\n";
	Pervasives.flush outc
    | SCons (x, stream) ->
	begin match x with
	  | `fragment f ->
	      fprintf outc "%S " f
	  | `break ->
	      fprintf outc "BREAK "
	  | `linebreak -> 
	      fprintf outc "LINEBREAK\n"
	  | `set_context c ->
	      fprintf outc "SET_CONTEXT(%s)" (context_to_string c)
	end;
	dump_raw outc stream

let rec dump outc stream =
  match Lazy.force stream with
    | SNil ->
	fprintf outc "\n";
	Pervasives.flush outc
    | SCons (x, stream) ->
	Array.iter
	  (function
	     | `fragment f ->
		 fprintf outc "%S " f
	     | `space n ->
		 fprintf outc "SPACE(%d) " n
	     | `set_context c ->
		 fprintf outc "SET_CONTEXT(%s) " (context_to_string c))
	  x;
	fprintf outc "\n";
	dump outc stream

let rec print ansi stream =
  match Lazy.force stream with
    | SNil ->
	flush ansi ()
    | SCons (x, stream) ->
	Array.iter
	  (function
	     | `fragment f ->
		 print_string ansi f
	     | `space n ->
		 print_space ansi n
	     | `set_context c ->
		 set_context ansi c)
	  x;
	print_newline ansi ();
	print ansi stream
