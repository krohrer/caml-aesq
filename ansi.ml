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
  Printf.sprintf "{I=%s,U=%s,N=%b,F=%s,B=%s}" 
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

let reset p () = 
  set_context p (make_context ())

(*----------------------------------------------------------------------------*)

open ExtLib

type 'a cell =
  | SNil
  | SCons of 'a * 'a stream
and 'a stream = 'a cell Lazy.t

type printable = [ `fragment of string | `space of int ]
type non_printable = [ `break | `linebreak | `set_context of context ]

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
let rec fill_line ~line ~break_space ~break_count ?(a=break_space) ~last =
  function
    | [] -> ()
    | `break::rest ->
	(* Bresenham stepping for greater accuracy! *)
	let a = a mod break_count + break_space in
	  line.(last) <- `space (a / break_count);
	  let last = last - 1 in
	    fill_line ~line ~break_space ~break_count ~a ~last rest
    | (`fragment _ as x)::rest
    | (`set_context _ as x)::rest ->
	line.(last) <- x;
	let last = last - 1 in
	  fill_line ~line ~break_space ~break_count ~a ~last rest

(* Measure line and possibly find last active context (for next line) *)
let rec measure_line ?(count=0) ?(break_count=0) ?(length=0) =
  function
    | [] -> count, break_count, length
    | x::rest ->
	let count = count + 1 in
	  match x with
	    | `fragment f ->
		let length = length + String.length f in
		  measure_line ~count ~break_count ~length rest
	    | `break ->
		let break_count = break_count + 1 in
		  measure_line ~count ~break_count ~length rest
	    | `set_context _ ->
		measure_line ~count ~break_count ~length rest

(* Return the context for the next line and an array of justified
   line elements *)
let justify_line ?(partial=false) ~width ~justification line_rev =
  let count, break_count, length = measure_line line_rev in
  let justification =
    match justification with
      | `block -> if partial || break_count = 0 then `left else `block
      | j -> j
  in
    match justification with
      | `left ->
	  (* Add left-over space on the right side *)
	  let space = width - length - break_count in
	  let break_space = break_count in
	  let line = Array.make (count + 1) (`space space) in
	    fill_line ~line ~break_space ~break_count ~last:(count-1) line_rev;
	    (* line.(count) <- `space space; *)
	    line
      | `center ->
	  (* Add a bit of left-over space on both sides *)
	  let space = width - length - break_count in
	  let break_space = break_count in
	  let left_space = space / 2 in
	  let right_space = space - left_space in
	  let line = Array.make (count + 2) (`space left_space) in
	    (* line.(0) <- `space left_space; *)
	    fill_line ~line ~break_space ~break_count ~last:count line_rev;
	    line.(count + 1) <- `space right_space;
	    line
      | `right ->
	  (* Add left-over space on the left side *)
	  let space = width - length - break_count in
	  let break_space = break_count in
	  let line = Array.make (count + 1) (`space space) in
	    (* line.(0) <- `space space; *)
	    fill_line ~line ~break_space ~break_count ~last:count line_rev;
	    line
      | `block ->
	  (* Distribute *)
	  assert (break_count > 0);
	  let break_space = width - length in
	  let line = Array.make count (`space 0) in
	    fill_line ~line ~break_space ~break_count ~last:(count-1) line_rev;
	    line

let format_min_width = 1 (* Minimum width allowed for formatting *)

let format' ?(width=78) ?(justification=`left) =
  (* Width and justification can be part of a closure, so we
     dont have to pass them along as parameters *)
  
  (* Collect line elements for justification *)
  let rec collect_line
      ~context
      (* Current context *)
      ?(rem_width=width)
      (* Remaining width *)
      ?(line_rev=[`set_context context])
      (* Reverse list of line elements, start each line explicitly
	 with the current context. Makes it easier to concat lines
	 later on. *)
      ?dismissables
      (* Line elements including elements after the last printable
	 that might be dismissed, if no printable was to
	 follow before the end of the line. *)
      stream
      =
    match Lazy.force stream with
      | SNil ->
	  (* Done *)
	  collect_done
	    ~context
	    ~line_rev
	    ~dismissables
	    ()
      | SCons (x, stream) ->
	  (* Dispatch *)
	  begin match x with
	    | `fragment _ as fragment ->
		collect_fragment
		  ~context
		  ~rem_width
		  ~line_rev
		  ~dismissables
		  fragment
		  stream
	    | `break ->
		collect_break
		  ~context
		  ~rem_width
		  ~line_rev
		  ~dismissables
		  stream
	    | `linebreak ->
		collect_linebreak
		  ~context
		  ~line_rev
		  stream
	    | `set_context _ as set_context ->
		collect_set_context
		  ~context
		  ~rem_width
		  ~line_rev
		  ~dismissables
		  set_context
		  stream
	  end
  
  (* Justify last line, if there are any printables. *)
  and collect_done
      ~context
      ~line_rev
      ~dismissables
      ()
      =
    let line =
      justify_line
	~partial:true
	~width
	~justification
	(Option.default line_rev dismissables)
    in
      SCons (line, lazy SNil)

  (* Collect fragment and justify line if necessary *)
  and collect_fragment
      ~context
      ~rem_width
      ~line_rev
      ~dismissables
      (`fragment frag as fragment)
      stream
      =
    let len = String.length frag in
      if len <= rem_width then
	(* Fragment still fits on this line *)
	collect_line
	  ~context
	  ~rem_width:(rem_width - len)
	  ~line_rev:(fragment :: Option.default line_rev dismissables)
	  stream
      else if len > width && rem_width >= format_min_width then
	(* Fragment must be split anyway, may as well start on this
	   line, if possible. It must always be possible if the line
	   does not yet contain any printable elements. *)
	let line =
	  let frag_left = String.slice ~last:rem_width frag in
	    justify_line
	      ~width
	      ~justification
	      (`fragment frag_left :: Option.default line_rev dismissables)
	and cell =
	  let frag_right = String.slice ~first:rem_width frag in
	    (* Prefix stream for next line with left-overs
	       from current line *)
	    SCons (`fragment frag_right,
		   stream)
	in
	  SCons (line,
		 lazy (collect_line
			 ~context
			 (lazy cell)))
      else
	(* Fragment does not fit on current line, retry on
	   next line. *)
	let line =
	  justify_line
	    ~width
	    ~justification
	    line_rev
	and cell =
	  (* This exact cell already exists, but we dont want to
	     pass it as an argument since it only gets used in this
	     case. So we simply construct it anew. (Thank science we
	     have immutable streams!) *)
	  SCons (fragment, stream)
	in
	  SCons (line,
		 lazy (collect_line ~context (lazy cell)))

  (* Break: Ignore at the beginning of the line, or add it to dismissables. *)
  and collect_break
      ~context
      ~rem_width
      ~line_rev
      ~dismissables
      stream
      =
    if width = rem_width then
      (* Simply ignore break at the beginning of the
	 line *)
      collect_line
	~context
	~rem_width
	~line_rev
	stream
    else
      (* Line already has more than one column *)
      begin match dismissables with
	| None ->
	    (* Add a dismissable break at cost of (at least) one
	       column, if there is still space left. *)
	    if rem_width > 1 then
	      let dismissables =
		`break :: Option.default line_rev dismissables
	      in
		collect_line
		  ~context
		  ~rem_width:(rem_width - 1)
		  ~line_rev
		  ~dismissables
		  stream
	    else
	      let line =
		justify_line
		  ~width
		  ~justification
		  line_rev
	      in
		SCons (line,
		       lazy (collect_line context stream))
	| Some _ ->
	    (* Already has dismissable break, ignore *)
	    collect_line
	      ~context
	      ~rem_width
	      ~line_rev
	      ?dismissables
	      stream
      end

  (* Linebreak: justify line without loose breaks and
     continue with next line *)
  and collect_linebreak
      ~context
      ~line_rev
      stream
      =
    let line = 
      justify_line
	~partial:true
	~width
	~justification
	line_rev
    in
      SCons (line,
	     lazy (collect_line context stream))

  (* Set context: Update current context and add it to the other elements *)
  and collect_set_context
      ~context
      ~rem_width
      ~line_rev
      ~dismissables
      (`set_context context as set_context) 
      stream
      =
    match dismissables with
      | None ->
	  (* Simply add to current line if we have no
	     loose breaks *)
	  collect_line
	    ~context
	    ~rem_width
	    ~line_rev:(set_context :: line_rev)
	    stream
      | Some breaks ->
	  (* We have one or more loose breaks after a
	     fragment, so we add it to that list. *)
	  collect_line
	    ~context
	    ~rem_width
	    ~line_rev
	    ~dismissables:(set_context :: breaks)
	    stream
  in
    collect_line

(* We do not want the stream to be in the closure above.  Otherwise,
   its cells would not be garbage collected until the stream was
   consumed entirely. *)
let format ?(width=78) ?(justification=`left) ?(context=make_context ()) stream =
  let width = max format_min_width width in
    lazy (format' ~width ~justification ~context stream)

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
	      fprintf outc "BR "
	  | `linebreak -> 
	      fprintf outc "LBR\n"
	  | `set_context c ->
	      fprintf outc "CTX(%s) " (context_to_string c)
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
		 fprintf outc "SP(%d) " n
	     | `set_context c ->
		 fprintf outc "CTX(%s) " (context_to_string c))
	  x;
	fprintf outc "\n";
	dump outc stream

let rec print ansi stream =
  match Lazy.force stream with
    | SNil ->
	reset ansi ();
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
