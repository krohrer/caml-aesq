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

type attributes = {
  c_intensity : intensity;
  c_underline : underline;
  c_inverted : bool;
  c_foreground : color;
  c_background : color;
}

let attributes_to_string c =
  Printf.sprintf "{I=%s,U=%s,N=%b,F=%s,B=%s}" 
    (intensity_to_string c.c_intensity)
    (underline_to_string c.c_underline)
    c.c_inverted
    (color_to_string c.c_foreground)
    (color_to_string c.c_background)

type formatter = {
  mutable p_intensity : intensity;
  mutable p_underline : underline;
  mutable p_inverted : bool;
  mutable p_foreground : color;
  mutable p_background : color;
  p_outc : out_channel;
  mutable p_attributes : attributes;
}

module Attributes =
struct
  type t = attributes

  let make
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

  let intensity attrs = attrs.c_intensity
  let underline attrs = attrs.c_underline
  let inverted attrs  = attrs.c_inverted
  let foreground attrs = attrs.c_foreground
  let background attrs = attrs.c_background

  let set_intensity i attrs  = { attrs with c_intensity = i }
  let set_underline u attrs  = { attrs with c_underline = u }
  let set_inverted i attrs   = { attrs with c_inverted = i }
  let set_foreground c attrs = { attrs with c_foreground = c }
  let set_background c attrs = { attrs with c_background = c }
end

let make_formatter ?attributes outc =
  let attributes =
    match attributes with
      | None -> Attributes.make ()
      | Some c -> c
  in
    {
      p_intensity = `normal;
      p_underline = `none;
      p_inverted = false;
      p_foreground = `default;
      p_background = `default;
      p_outc = outc;
      p_attributes = attributes;
    }

let default_formatter = make_formatter stdout

let attributes p =
  p.p_attributes

let set_attributes p attrs =
  p.p_attributes <- attrs

let map_attributes p f =
  p.p_attributes <- f p.p_attributes

let enforce_intensity p codes =
  let i = p.p_attributes.c_intensity in
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
  let u = p.p_attributes.c_underline in
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
  let i = p.p_attributes.c_inverted in
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
  let c = p.p_attributes.c_foreground in
    if c <> p.p_foreground then begin
      p.p_foreground <- c;
      (color_code 30 c) :: codes
    end else
      codes

let enforce_background p codes =
  let c = p.p_attributes.c_background in
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

let formatter_flush p () =
  enforce_attributes p ();
  Pervasives.flush p.p_outc

let formatter_reset p () = 
  set_attributes p (Attributes.make ())

(*----------------------------------------------------------------------------*)

module Text =
struct
  open ExtLib
  open LazyStream

  type printable = [ `fragment of string | `space of int ]
  type non_printable = [ `break | `linebreak | `set_attributes of attributes ]

  type raw = [ `fragment of string | `break | `linebreak | `set_attributes of attributes ]
  type linel = [ `fragment of string | `space of int | `set_attributes of attributes]

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
      | (`set_attributes _ as x)::rest ->
	  line.(last) <- x;
	  let last = last - 1 in
	    fill_line ~line ~break_space ~break_count ~a ~last rest

  (* Measure line and possibly find last active attributes (for next line) *)
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
	      | `set_attributes _ ->
		  measure_line ~count ~break_count ~length rest

  (* Return an array of justified line elements *)
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
	~attributes
	(* Current attributes *)
	?(rem_width=width)
	(* Remaining width *)
	?(line_rev=[`set_attributes attributes])
	(* Reverse list of line elements, start each line explicitly
	   with the current attributes. Makes it easier to concat lines
	   later on. *)
	?dismissables
	(* Line elements including elements after the last printable
	   that might be dismissed, if no printable was to
	   follow before the end of the line. *)
	stream
	=
      match Lazy.force stream with
	| Nil ->
	    (* Done *)
	    collect_done
	      ~attributes
	      ~line_rev
	      ~dismissables
	      ()
	| Cons (x, stream) ->
	    (* Dispatch *)
	    begin match x with
	      | `fragment _ as fragment ->
		  collect_fragment
		    ~attributes
		    ~rem_width
		    ~line_rev
		    ~dismissables
		    fragment
		    stream
	      | `break ->
		  collect_break
		    ~attributes
		    ~rem_width
		    ~line_rev
		    ~dismissables
		    stream
	      | `linebreak ->
		  collect_linebreak
		    ~attributes
		    ~line_rev
		    stream
	      | `set_attributes _ as set_attributes ->
		  collect_set_attributes
		    ~attributes
		    ~rem_width
		    ~line_rev
		    ~dismissables
		    set_attributes
		    stream
	    end
	      
    (* Justify last line, if there are any printables. *)
    and collect_done
	~attributes
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
	Cons (line, lazy Nil)

    (* Collect fragment and justify line if necessary *)
    and collect_fragment
	~attributes
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
	    ~attributes
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
	      Cons (`fragment frag_right,
		    stream)
	  in
	    Cons (line,
		  lazy (collect_line
			  ~attributes
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
	    Cons (fragment, stream)
	  in
	    Cons (line,
		  lazy (collect_line ~attributes (lazy cell)))

    (* Break: Ignore at the beginning of the line, or add it to dismissables. *)
    and collect_break
	~attributes
	~rem_width
	~line_rev
	~dismissables
	stream
	=
      if width = rem_width then
	(* Simply ignore break at the beginning of the
	   line *)
	collect_line
	  ~attributes
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
		    ~attributes
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
		  Cons (line,
			lazy (collect_line attributes stream))
	  | Some _ ->
	      (* Already has dismissable break, ignore *)
	      collect_line
		~attributes
		~rem_width
		~line_rev
		?dismissables
		stream
	end

    (* Linebreak: justify line without loose breaks and
       continue with next line *)
    and collect_linebreak
	~attributes
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
	Cons (line,
	      lazy (collect_line attributes stream))

    (* Set attributes: Update current attributes and add it to the other elements *)
    and collect_set_attributes
	~attributes
	~rem_width
	~line_rev
	~dismissables
	(`set_attributes attributes as set_attributes) 
	stream
	=
      match dismissables with
	| None ->
	    (* Simply add to current line if we have no
	       loose breaks *)
	    collect_line
	      ~attributes
	      ~rem_width
	      ~line_rev:(set_attributes :: line_rev)
	      stream
	| Some breaks ->
	    (* We have one or more loose breaks after a
	       fragment, so we add it to that list. *)
	    collect_line
	      ~attributes
	      ~rem_width
	      ~line_rev
	      ~dismissables:(set_attributes :: breaks)
	      stream
    in
      collect_line

  (* We do not want the stream to be in the closure above.  Otherwise,
     its cells would not be garbage collected until the stream was
     consumed entirely. *)
  let format
      ?(width=78)
      ?(justification=`left)
      ?(attributes=Attributes.make ())
      stream
      =
    let width = max format_min_width width in
      lazy (format' ~width ~justification ~attributes stream)

  (*----------------------------------------------------------------------------*)

  open Printf

  let rec dump_raw outc stream =
    match Lazy.force stream with
      | Nil ->
	  fprintf outc "\n";
	  Pervasives.flush outc
      | Cons (x, stream) ->
	  begin match x with
	    | `fragment f ->
		fprintf outc "%S " f
	    | `break ->
		fprintf outc "BR "
	    | `linebreak -> 
		fprintf outc "LBR\n"
	    | `set_attributes c ->
		fprintf outc "ATTRS(%s) " (attributes_to_string c)
	  end;
	  dump_raw outc stream

  let rec dump outc stream =
    match Lazy.force stream with
      | Nil ->
	  fprintf outc "\n";
	  Pervasives.flush outc
      | Cons (x, stream) ->
	  Array.iter
	    (function
	       | `fragment f ->
		   fprintf outc "%S " f
	       | `space n ->
		   fprintf outc "SP(%d) " n
	       | `set_attributes c ->
		   fprintf outc "ATTRS(%s) " (attributes_to_string c))
	    x;
	  fprintf outc "\n";
	  dump outc stream

  let rec print ansi stream =
    match Lazy.force stream with
      | Nil ->
	  formatter_reset ansi ();
	  formatter_flush ansi ()
      | Cons (x, stream) ->
	  Array.iter
	    (function
	       | `fragment f ->
		   print_string ansi f
	       | `space n ->
		   print_space ansi n
	       | `set_attributes c ->
		   set_attributes ansi c)
	    x;
	  print_newline ansi ();
	  print ansi stream

end
