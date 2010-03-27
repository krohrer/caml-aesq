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
  Printf.sprintf "{int=%s, ul=%s, inv=%b, fg=%s, bg=%s}" 
    (intensity_to_string c.c_intensity)
    (underline_to_string c.c_underline)
    c.c_inverted
    (color_to_string c.c_foreground)
    (color_to_string c.c_background)

type t = {
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

type 'a stream_cell =
  | SNil
  | SCons of 'a * 'a stream
and 'a stream = 'a stream_cell Lazy.t

let rec sappend s sapp =
  match Lazy.force s with
    | SNil -> sapp
    | SCons (x, s) ->
	lazy (SCons (x, sappend s sapp))

let sflatten slist =
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

type ops = [ 
| `nop
| `set_context of context
| `set_intensity of intensity
| `set_underline of underline
| `set_inverted of bool
| `set_foreground of color
| `set_background of color
]

type format_ops = [
| `push_justification of justification
| `push_context of context
| `push_width of int
| `pop_justification
| `pop_context
| `pop_width
]

type frag = [ `fragment of string ]
type breaks = [ `break | `linebreak ]
type whitespaces = [ `space of int | `newline ]

(*----------------------------------------------------------------------------*)

module LineSplitter =
struct
  type input  = [ frag | breaks | ops ]
  type output = [ frag | breaks | ops ]

  let rec split' width ~rem_width ?(has_break=false) stream =
    match Lazy.force stream with
      | SNil ->
          SNil
      | SCons (x, stream) ->
          match x with
            | `fragment f ->
                (* Handle fragments separately *)
                split_fragment width ~rem_width ~needs_break:has_break f stream
            | `break ->
		let has_break = rem_width <> width in
                  (* Ignore break at beginning of line *)
		  split' width ~rem_width ~has_break stream
            | (`linebreak as x) ->
                (* Explicit line break, make it double*)
		let sapp = lazy (split' width ~rem_width:width stream) in
                  SCons (x, lazy (SCons (x, sapp)))
            | #ops as x ->
                (* Passthrough ops *)
		let sapp = lazy (split' width ~rem_width stream) in
                  SCons (x, sapp)
  and split_fragment width ~rem_width ~needs_break frag stream =
    let flen = String.length frag in
    let blen = if needs_break then flen + 1 else flen in
      if blen <= rem_width  then
	(* fragment (and possibly break) still fit on this line *)
	let sapp = lazy (split' width ~rem_width:(rem_width - blen) stream) in
	let s = SCons (`fragment frag, sapp)
	in
	  if needs_break then
	    SCons (`break, lazy s)
	  else
	    s
      else if flen > width then
	(* fragment must be split anyway, may as well start on this line *)
	let fleft = String.slice ~last:rem_width frag in
	let fright = String.slice ~first:rem_width frag in
	let sapp = lazy (split_fragment width ~rem_width:width ~needs_break:false fright stream) in
	let s = SCons (`fragment fleft, lazy (SCons (`linebreak, sapp)))
	in
	  if needs_break then
	    SCons (`break, lazy s)
	  else
	    s
    else
      (* fragment fits on next line for sure *)
      let sapp = lazy (split_fragment width ~rem_width:width ~needs_break:false frag stream) in
	SCons (`linebreak, sapp)

  let split ~width stream =
    lazy (split' ~rem_width:width width stream)
end

(*----------------------------------------------------------------------------*)

module Justification =
struct
  type input  = [ frag | breaks | ops ]
  type output = [ frag | whitespaces | ops ]

  let rec collect_line accum_rev width just stream =
    (* Collect non-linebreak elements for later justification  *)
    match Lazy.force stream with
      | SNil ->
          justify_line ~special:true width just (List.rev accum_rev) (lazy SNil)
      | SCons (x, stream) ->
          match x with
            | `linebreak -> begin
                (* Justify accumulated line *)
		match Lazy.force stream with
		  | SCons (`linebreak, stream) ->
		      (* Block justification is special *)
		      let sapp = lazy (collect_line [] width just stream) in
			justify_line ~special:true width just (List.rev accum_rev) sapp
		  | SNil ->
		      (* Block justification is special *)
		      let sapp = lazy (collect_line [] width just stream) in
			justify_line width just (List.rev accum_rev) sapp
		  | _ ->
		      let sapp = lazy (collect_line [] width just stream) in
			justify_line width just (List.rev accum_rev) sapp
	      end
            | (`break as x)
            | (`fragment _ as x)
            | (#ops as x) ->
                (* Collect elements for current line *)
                collect_line (x::accum_rev) width just stream
  and justify_fix left_space right_space accum sapp =
    (* Justify with fixed space left and right *)
    let rec make_stream = function
      | [] ->
          (* Space on the right *)
          SCons (`space right_space,
		 lazy (SCons (`newline, sapp)))
      | `break::rest ->
          (* Break is one space worth (fixed) *)
          SCons (`space 1, lazy (make_stream rest))
      | (`fragment _ as x)::rest
      | (#ops as x)::rest ->
          (* Passthrough for fragments and ops *)
          SCons (x, lazy (make_stream rest))
    in
      (* Space on the left *)
      SCons (`space left_space, lazy (make_stream accum))
  and justify_left rem_space =
    justify_fix 0 rem_space
  and justify_center rem_space =
    let hr = rem_space / 2 in
      justify_fix hr (rem_space - hr)
  and justify_right rem_space =
    justify_fix rem_space 0
  and justify_block breaks rem_space accum sapp =
    (* Justify space evenly for all breaks *)
    let rec make_stream a = function
      | [] ->
          SCons (`newline, sapp)
      | `break::rest ->
          (* Use integer arithmetic instead of float *)
          SCons (`space (a / breaks),
                 lazy (make_stream ((a mod breaks) + rem_space) rest))
      | (`fragment _ as x)::rest
      | (#ops as x)::rest ->
          (* Passthrough for fragments and ops *)
          SCons (x, lazy (make_stream a rest))
    in
      make_stream rem_space accum
  and justify_line ?(special=false) width just accum sapp =
    (* Dispatch justification algorithm *)
    let break_count, frag_len = measure_line accum in
    let rem_width = max 0 (width - frag_len - break_count) in
      match just with
        | `left ->
            justify_left rem_width accum sapp
        | `center ->
            justify_center rem_width accum sapp
        | `right ->
            justify_right rem_width accum sapp
        | `block ->
	    if special then
	      justify_left rem_width accum sapp
	    else
              justify_block break_count (rem_width + break_count) accum sapp
  and measure_line accum =
    (* Count breaks and measure fragment length *)
    let rec fold break_count frag_len =
      function
        | [] ->
            break_count, frag_len
        | `break::rest ->
            fold (break_count + 1) frag_len rest
        | `fragment f::rest ->
            fold break_count (frag_len + String.length f) rest
        | #ops::rest ->
            fold break_count frag_len rest
    in
      fold 0 0 accum

  let justify ~width just stream =
    match Lazy.force stream with
      | SNil -> lazy SNil
      | _ -> lazy (collect_line [] width just stream)
end

(*----------------------------------------------------------------------------*)

module Format =
struct
  type input  = [ `fragment of string | `break | `linebreak | `context of context ]
  type output = [ `fragment of string | `space of int | `context of context ]

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
      | (`context _ as x)::rest ->
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
	      | `context c ->
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
	    let line = Array.make (count + 2) (`context context) in
	      (* line.(0) <- `context context; *)
	      fill_line ~line ~break_space ~break_count count line_rev;
	      line.(count + 1) <- `space space;
	      line
	| `center ->
	    (* Add a bit of left-over space on both sides *)
	    let space = width - length - break_count in
	    let break_space = break_count in
	    let left_space = space / 2 in
	    let right_space = space - left_space in
	    let line = Array.make (count + 3) (`context context) in
	      (* line.(0) <- `context context; *)
	      line.(1) <- `space left_space;
	      fill_line ~line ~break_space ~break_count (count + 1) line_rev;
	      line.(count + 2) <- `space right_space;
	      line
	| `right ->
	    (* Add left-over space on the left side *)
	    let space = width - length - break_count in
	    let break_space = break_count in
	    let line = Array.make (count + 2) (`context context) in
	      (* line.(0) <- `context context; *)
	      line.(1) <- `space space;
	      fill_line ~line ~break_space ~break_count (count + 1) line_rev;
	      line
	| `block ->
	    (* Distribute *)
	    assert (break_count > 0);
	    let break_space = width - length in
	    let line = Array.make (count + 2) (`context context) in
	      (* line.(0) <- `context context; *)
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
	      | `context _ as x ->
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
end

(*----------------------------------------------------------------------------*)

module Columns =
struct
  type input  = [ frag | whitespaces | ops ]
  type output = [ frag | whitespaces | ops ]

  let make _ = lazy SNil
end

(*----------------------------------------------------------------------------*)

module Debug =
struct
  type input = [ frag | whitespaces | breaks | ops | format_ops ]

  open Printf

  let rec dump' outc justs widths ctxs stream =
    match Lazy.force stream with
      | SNil ->
	  Pervasives.flush outc
      | SCons (#format_ops as x, stream) ->
	  dump_format_ops outc justs widths ctxs stream x
      | SCons (#ops as x, stream)
      | SCons (#frag as x, stream)
      | SCons (#breaks as x, stream)
      | SCons (#whitespaces as x, stream) ->
	  begin match x with 
	    | `nop ->
		fprintf outc "NOP "
	    | `set_intensity i ->
		fprintf outc "INTENSITY(%s) " (intensity_to_string i);
	    | `set_underline u ->
		fprintf outc "UNDERLINE(%s) " (underline_to_string u);
	    | `set_inverted i ->
		fprintf outc "INVERTED(%b) " i
	    | `set_foreground c ->
		fprintf outc "FOREGROUND(%s) " (color_to_string c);
	    | `set_background c ->
		fprintf outc "BACKGROUND(%s) " (color_to_string c);
	    | `set_context c ->
		fprintf outc "CONTEXT(%s) " (context_to_string c);
            | `fragment f ->
		fprintf outc "%S " f;
            | `break ->
		fprintf outc "BR ";
            | `linebreak ->
		fprintf outc "LBR\n";
            | `space n ->
		fprintf outc "S(%d) " n;
            | `newline ->
		fprintf outc "NL\n";
	  end;
    	  dump' outc justs widths ctxs stream
  and dump_format_ops outc justs widths ctxs stream =
    function
      | `push_justification j ->
	  fprintf outc "\nPUSH JUSTIFICATION(%s)\n" (justification_to_string j);
	  dump' outc (j::justs) widths ctxs stream
      | `pop_justification ->
	  begin match justs with
	    | [] ->
		fprintf outc "\nPOP JUSTIFICATION: STACK EMPTY!\n";
		dump' outc justs widths ctxs stream
	    | j::justs -> 
		fprintf outc "\nPOP JUST -> %s\n" (justification_to_string j);
		dump' outc justs widths ctxs stream
	  end
      | `push_context c ->
	  fprintf outc "\nPUSH CONTEXT\n";
	  dump' outc justs widths (c::ctxs) stream
      | `pop_context ->
	  begin match ctxs with
	    | [] ->
		fprintf outc "\nPOP CONTEXTS: STACK EMPTY!\n";
		dump' outc justs widths ctxs stream
	    | c::ctxs ->
		fprintf outc "\nPOP CONTEXT\n";
		dump' outc justs widths ctxs stream
	  end
      | `push_width w ->
	  fprintf outc "\nPUSH WIDTH(%d)\n" w;
	  dump' outc justs (w::widths) ctxs stream
      | `pop_width ->
	  begin match widths with
	    | [] ->
		fprintf outc "\nPOP WIDTH: STACK EMPTY\n";
	    | w::widths ->
		fprintf outc "\nPOP WIDTH -> %d\n" w;
		dump' outc justs widths ctxs stream
	  end

  let dump outc stream =
    dump' outc [] [] [] stream
end

(*----------------------------------------------------------------------------*)

module Printer =
struct
  type input = [ frag | whitespaces | breaks | ops ]

  let rec print ansi stream =
    match Lazy.force stream with
      | SNil -> flush ansi ()
      | SCons (c, s) ->
          begin match c with
            | `fragment f ->
                print_string ansi f
            | #ops as o ->
                print_ops ansi o
            | `space n ->
                print_space ansi n
            | `newline ->
                print_newline ansi ()
            | `break ->
                print_string ansi "_"
            | `linebreak ->
                print_string ansi "\\\n"
          end;
          print ansi s
  and print_ops ansi =
    function
      | `nop ->
          ()
      | `set_intensity i ->
	  map_context ansi (set_intensity i)
      | `set_underline u ->
	  map_context ansi (set_underline u)
      | `set_inverted i ->
	  map_context ansi (set_inverted i)
      | `set_foreground c ->
	  map_context ansi (set_foreground c)
      | `set_background c ->
	  map_context ansi (set_background c)
      | `set_context ctx ->
          set_context ansi ctx
end
