type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [ `faint | `normal | `bold ]
type justification = [ `none | `left | `center | `right | `block ]
type underline = [ `single | `none ]

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

let default_attributes = Attributes.make ()
let std_formatter = make_formatter stdout

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

let flush p () =
  set_attributes p (Attributes.make ());
  enforce_attributes p ();
  Pervasives.flush p.p_outc

(*----------------------------------------------------------------------------*)

module Text =
struct
  open ExtLib

  type printable = [ `fragment of string | `space of int ]
  type non_printable = [ `break | `linebreak | `attributes of attributes ]


  type raw     = [ `fragment of string | `break | `linebreak | `attributes of attributes ]
  type chopped = [ `fragment of string | `break              | `attributes of attributes ]
  type cooked  = [ `fragment of string | `space of int       | `attributes of attributes ]

  type width = int
  type line = cooked array * width

  let calc_line_width elements =
    let aux sum =
      function
	| `fragment f -> sum + String.length f
	| `space n -> sum + n
	| _ -> sum
    in
      Array.fold_left aux 0 elements

  let empty_line =
    ([||], 0)

  let make_line elements =
    (elements, calc_line_width elements)

  let line_width (_,width) = width

  let line_concat lines =
    let elements_array, widths = List.split lines in
    let elements = Array.concat elements_array in
    let width = List.fold_left (+) 0 widths in
      (elements, width)

  let min_width = 1 (* Minimum width allowed for formatting *)

  (* Chop text up into lines (lines still need to be cooked tough) *)
  let rec chop attributes width stream =
    let width = max min_width width in
      chop' width attributes stream

  (* Closures are a poor mans objects, and [width] is the only
     constant field. Stream is not part of it, because it would
     prevent the stream cells from getting gc'd. *)
  and chop' width =
    (* Chop text so its lines are no longer than [width] *)
    let rec chop_line
	~attributes
	(* Current attributes *)
	?(rem_width=width)
	(* Remaining width *)
	?(line_rev=[`attributes attributes])
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
	| LazyStream.Nil ->
	    (* Done *)
	    chop_done
	      ~attributes
	      ~line_rev
	      ~dismissables
	      ()
	| LazyStream.Cons (x, stream) ->
	    (* Dispatch *)
	    begin match x with
	      | `fragment _ as x ->
		  chop_fragment
		    ~attributes
		    ~rem_width
		    ~line_rev
		    ~dismissables
		    x
		    stream
	      | `break ->
		  chop_break
		    ~attributes
		    ~rem_width
		    ~line_rev
		    ~dismissables
		    stream
	      | `linebreak ->
		  chop_linebreak
		    ~attributes
		    ~line_rev
		    stream
	      | `attributes _ as x ->
		  chop_set_attributes
		    ~attributes
		    ~rem_width
		    ~line_rev
		    ~dismissables
		    x
		    stream
	    end
	      
    (* Make partial line from the remaining elements *)
    and chop_done
	~attributes
	~line_rev
	~dismissables
	()
	=
      let line =
	make_chopped
	  ~partial:true
	  (Option.default line_rev dismissables)
      in
	LazyStream.Cons (line, lazy LazyStream.Nil)

    (* Chop fragment and justify line if necessary *)
    and chop_fragment
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
	  chop_line
	    ~attributes
	    ~rem_width:(rem_width - len)
	    ~line_rev:(fragment :: Option.default line_rev dismissables)
	    stream
	else if len > width && rem_width >= min_width then
	  (* Fragment must be split anyway, may as well start on this
	     line, if possible. It must always be possible if the line
	     does not yet contain any printable elements. *)
	  let line =
	    let frag_left = String.slice ~last:rem_width frag in
	      make_chopped
		(`fragment frag_left :: Option.default line_rev dismissables)
	  and cell =
	    let frag_right = String.slice ~first:rem_width frag in
	      (* Prefix stream for next line with left-overs
		 from current line *)
	      LazyStream.Cons (`fragment frag_right,
			       stream)
	  in
	    LazyStream.Cons (line,
			     lazy (chop_line
				     ~attributes
				     (lazy cell)))
	else
	  (* Fragment does not fit on current line, retry on
	     next line. *)
	  let line =
	    make_chopped line_rev
	  and cell =
	    (* This exact cell already exists, but we dont want to
	       pass it as an argument since it only gets used in this
	       case. So we simply construct it anew. (Thank science we
	       have immutable streams!) *)
	    LazyStream.Cons (fragment, stream)
	  in
	    LazyStream.Cons (line,
			     lazy (chop_line ~attributes (lazy cell)))

    (* Break: Ignore at the beginning of the line, or add it to dismissables. *)
    and chop_break
	~attributes
	~rem_width
	~line_rev
	~dismissables
	stream
	=
      if width = rem_width then
	(* Simply ignore break at the beginning of the
	   line *)
	chop_line
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
		  chop_line
		    ~attributes
		    ~rem_width:(rem_width - 1)
		    ~line_rev
		    ~dismissables
		    stream
	      else
		let line =
		  make_chopped line_rev
		in
		  LazyStream.Cons (line,
				   lazy (chop_line attributes stream))
	  | Some _ ->
	      (* Already has dismissable break, ignore *)
	      chop_line
		~attributes
		~rem_width
		~line_rev
		?dismissables
		stream
	end

    (* Linebreak: justify line without loose breaks and
       continue with next line *)
    and chop_linebreak
	~attributes
	~line_rev
	stream
	=
      let line = 
	make_chopped ~partial:true line_rev
      in
	LazyStream.Cons (line,
			 lazy (chop_line attributes stream))

    (* Set attributes: Update current attributes and add it to the other elements *)
    and chop_set_attributes
	~attributes
	~rem_width
	~line_rev
	~dismissables
	(`attributes attributes as x)
	stream
	=
      match dismissables with
	| None ->
	    (* Simply add to current line if we have no
	       loose breaks *)
	    chop_line
	      ~attributes
	      ~rem_width
	      ~line_rev:(x :: line_rev)
	      stream
	| Some breaks ->
	    (* We have one or more loose breaks after a
	       fragment, so we add it to that list. *)
	    chop_line
	      ~attributes
	      ~rem_width
	      ~line_rev
	      ~dismissables:(x :: breaks)
	      stream

    (* Make a chopped line *)
    and make_chopped
	?(partial=false)
	line_rev
	=
      let line = Array.of_list line_rev in
	Array.rev_in_place line;
	line, partial
    in
      fun attributes stream -> lazy (chop_line ~attributes stream)

  (*------------------------------------*)
	
  (* Convert break to space of width 1, leave the rest as is *)
  let break_to_space =
    function
      | `fragment _ | `attributes _ as x -> x
      | `break -> `space 1

  (* Convert breaks to spaces by distributing [break_space] evenly
     among all spaces. Because widths are integers, we use something
     similar to the Bresenham line-drawing algorithm *)
  let break_to_space' ~break_space ~break_count () =
    let a = ref break_space in
      function
	| `fragment _ | `attributes _ as x -> x
	| `break ->
	    let x = `space (!a / break_count) in
	      a := !a mod break_count + break_space;
	      x
      
  (* Measure total width of all printable elements *)
  let measure_fragments elements =
    let aux sum =
      function
	| `fragment f -> sum + String.length f
	| _ -> sum
    in
      Array.fold_left aux 0 elements

  (* Count number of breaks *)
  let count_breaks elements =
    let aux sum =
      function
	| `break -> sum + 1
	| _ -> sum
    in
      Array.fold_left aux 0 elements

  (* Return an array of justified line elements *)
  let justify_line width justification (elements, partial) =
    let break_count = count_breaks elements in
    let fragment_width = measure_fragments elements in
    let justification =
      match justification with
	| `block -> if partial || break_count = 0 then `left else `block
	| j -> j
    in
      match justification with
	| `none ->
	    (* Simply convert breaks to spaces, ignore desired width *)
	    (Array.map break_to_space elements, fragment_width + break_count)
	| `left ->
	    (* Add left-over space on the right side *)
	    let space = max 0 (width - fragment_width - break_count) in
	    let count = Array.length elements + 1 in
	    let line = Array.make count (`space space) in
	    let aux i x = line.(i) <- break_to_space x in
	      Array.iteri aux elements;
	      (line, width)
	| `center ->
	    (* Add a bit of left-over space on both sides *)
	    let space = max 0 (width - fragment_width - break_count) in
	    let left_space = space / 2 in
	    let right_space = space - left_space in
	    let count = Array.length elements + 2 in
	    let line = Array.make count (`space left_space) in
	    let aux i x = line.(i+1) <- break_to_space x in
	      (* line.(0) <- `space left_space; *)
	      Array.iteri aux elements;
	      line.(count-1) <- `space right_space;
	      (line, width)
	| `right ->
	    (* Add left-over space on the left side *)
	    let space = max 0 (width - fragment_width - break_count) in
	    let count = Array.length elements + 1 in
	    let line = Array.make count (`space space) in
	    let aux i x = line.(i+1) <- break_to_space x in
	      (* line.(0) <- `space space; *)
	      Array.iteri aux elements;
	      (line, width)
	| `block ->
	    (* Distribute *)
	    assert (break_count > 0);
	    let break_space = width - fragment_width in
	    let aux = break_to_space' ~break_space ~break_count () in
	    let line = Array.map aux elements in
	      (line, width)

  let format
      ?(attributes=default_attributes)
      ?(width=78)
      ?(justification=`none)
      stream
      =
    let chopped_stream = chop attributes width stream in
      LazyStream.map (justify_line width justification) chopped_stream

  (*----------------------------------------------------------------------------*)

  let tabulate ?(separator=empty_line) tabs =
    lazy LazyStream.Nil

  (*----------------------------------------------------------------------------*)

  open Printf

  let rec dump_raw outc stream =
    match Lazy.force stream with
      | LazyStream.Nil ->
	  fprintf outc "\n";
	  Pervasives.flush outc
      | LazyStream.Cons (x, stream) ->
	  begin match x with
	    | `fragment f ->
		fprintf outc "%S " f
	    | `break ->
		fprintf outc "BR "
	    | `linebreak -> 
		fprintf outc "LBR\n"
	    | `attributes c ->
		fprintf outc "ATTRS(%s) " (attributes_to_string c)
	  end;
	  dump_raw outc stream

  let rec dump outc stream =
    match Lazy.force stream with
      | LazyStream.Nil ->
	  fprintf outc "\n";
	  Pervasives.flush outc
      | LazyStream.Cons ((x,_), stream) ->
	  Array.iter
	    (function
	       | `fragment f ->
		   fprintf outc "%S " f
	       | `space n ->
		   fprintf outc "SP(%d) " n
	       | `attributes c ->
		   fprintf outc "ATTRS(%s) " (attributes_to_string c))
	    x;
	  fprintf outc "\n";
	  dump outc stream

  let rec print ansi stream =
    match Lazy.force stream with
      | LazyStream.Nil ->
	  flush ansi ()
      | LazyStream.Cons ((x,_), stream) ->
	  Array.iter
	    (function
	       | `fragment f ->
		   print_string ansi f
	       | `space n ->
		   print_space ansi n
	       | `attributes c ->
		   set_attributes ansi c)
	    x;
	  print_newline ansi ();
	  print ansi stream

end
