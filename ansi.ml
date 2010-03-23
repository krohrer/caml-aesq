type color = [`black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default]
type intensity = [`faint | `normal | `bold]
type justification = [`left | `center | `right | `block ]
type underline = [`single | `none]

type context = {
  mutable c_intensity : intensity;
  mutable c_underline : underline;
  mutable c_inverted : bool;
  mutable c_foreground : color;
  mutable c_background : color;
}

type t = {
  p_outc : out_channel;
  p_real : context;
  p_default : context;
  mutable p_active : context;
}

let make_context () = {
  c_intensity = `normal;
  c_underline = `none;
  c_inverted = false;
  c_foreground = `default;
  c_background = `default;
}

let copy_context ctx = {
  c_intensity = ctx.c_intensity;
  c_underline = ctx.c_underline;
  c_inverted = ctx.c_inverted;
  c_foreground = ctx.c_foreground;
  c_background = ctx.c_background
}

let default_context p =
  p.p_default

let make outc =
  let defctx = make_context () in {
      p_outc = outc;
      p_real = make_context ();
      p_default = defctx;
      p_active = defctx;
    }

let reset p () =
  let ctx = p.p_default in
    ctx.c_intensity <- `normal;
    ctx.c_underline <- `none;
    ctx.c_inverted <- false;
    ctx.c_foreground <- `default;
    ctx.c_background <- `default;
    p.p_active <- ctx

let set_intensity p i =
  p.p_active.c_intensity <- i
  
let set_underline p u = 
  p.p_active.c_underline <- u

let set_inverted p i =
  p.p_active.c_inverted <- i

let set_foreground p c =
  p.p_active.c_foreground <- c

let set_background p c =
  p.p_active.c_background <- c

let set_context p ctx = 
  p.p_active <- ctx

let enforce_intensity p codes =
  let i = p.p_active.c_intensity in
  let real = p.p_real in
    if i <> real.c_intensity then begin
      real.c_intensity <- i;
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
  let u = p.p_active.c_underline in
  let real = p.p_real in
    if u <> real.c_underline then begin
      real.c_underline <- u;
      let c =
	match u with
	  | `single -> 4 
	  | `none -> 24
      in
	c :: codes
    end else
      codes

let enforce_inverted p codes =
  let i = p.p_active.c_inverted in
  let real = p.p_real in
    if i <> real.c_inverted then begin
      real.c_inverted <- i;
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
  let c = p.p_active.c_foreground in
  let real = p.p_real in
    if c <> real.c_foreground then begin
      real.c_foreground <- c;
      (color_code 30 c) :: codes
    end else
      codes

let enforce_background p codes =
  let c = p.p_active.c_background in
  let real = p.p_real in
    if c <> real.c_background then begin
      real.c_background <- c;
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

type ops = [ 
| `set_intensity of intensity
| `set_underline of underline
| `set_inverted of bool
| `set_foreground of color
| `set_background of color
| `set_context of context
]

type frag = [ `fragment of string ]
type breaks = [ `break | `linebreak ]
type whitespaces = [ `space of int | `newline ]

(*----------------------------------------------------------------------------*)

module LineSeparation =
struct
  type input  = [ frag | breaks | ops ] Stream.t
  type output = [ frag | breaks | ops ] Stream.t

  let rec splitter width ~rem_width stream () =
    match Stream.peek stream with
      | None -> Stream.sempty
      | Some c ->
	  match c with
	    | `fragment f ->
		(* Handle fragments separately *)
		split_fragment width ~rem_width f stream ()
	    | `break ->
		(* Normalize breaks *)
		accumulate_breaks width ~rem_width stream ()
	    | (`linebreak as x) ->
		(* Start a new line *)
		Stream.icons x (Stream.slazy (splitter width ~rem_width:width stream))
	    | #ops as x ->
		(* Passthrough ops *)
		Stream.icons x (Stream.slazy (splitter width ~rem_width stream))
  and accumulate_breaks width ~rem_width stream () =
    match Stream.peek stream with
      | None -> Stream.sempty
      | Some c ->
	  match c with
	    | `fragment f ->
		if rem_width <= 2 then
		  (* Break the line if not enough space left *)
		  Stream.icons `linebreak (Stream.slazy (split_fragment width ~rem_width:width f stream))
		else if rem_width = width then
		  (* Ignore break at beginning of line *)
		  split_fragment width ~rem_width f stream ()
		else
		  (* Insert a break *)
		  Stream.icons `break (Stream.slazy (split_fragment width ~rem_width:(rem_width-1) f stream))
	    | `break ->
		(* Just another break in the wall (stream) *)
		accumulate_breaks width ~rem_width stream ()
	    | (`linebreak as x) ->
		(* Ignore breaks and start new line*)
		Stream.icons x (Stream.slazy (splitter width ~rem_width:width stream))
	    | #ops as x ->
		(* Pass-through ops *)
		Stream.icons x (Stream.slazy (accumulate_breaks width ~rem_width stream))
  and split_fragment width ~rem_width frag stream () =
    let flen = String.length frag in
      if flen < rem_width then
	(* fragment still fits on this line *)
	Stream.icons (`fragment frag) (Stream.slazy (splitter width ~rem_width:(rem_width - flen) stream))
      else if flen > width then
	(* fragment must be split anyway, may as well start on this line *)
	let left = String.slice ~last:rem_width frag in
	let right = String.slice ~first:rem_width frag in
	  Stream.icons
	    (`fragment left)
	    (Stream.icons
	       `linebreak
	       (Stream.slazy (split_fragment width ~rem_width:width right stream)))
      else
	(* fragment fits on next line for sure *)
	Stream.icons
	  `linebreak
	  (Stream.slazy (split_fragment width ~rem_width:width frag stream))

  let separate ~width stream =
    Stream.slazy (splitter ~rem_width:width width stream)
end

(*----------------------------------------------------------------------------*)

module Justification =
struct
  type input  = [ frag | breaks | ops ] Stream.t
  type output = [ frag | whitespaces | ops ] Stream.t

  let rec collect_line accum_rev width just stream () =
    (* Collect non-linebreak elements for later justification *)
    match Stream.peek stream with
      | None ->
	  justify_line width just (List.rev accum_rev) (Stream.slazy (collect_line [] width just stream))
      | Some c ->
	  match c with
	    | `linebreak ->
		(* Justify accumulated line *)
		justify_line width just (List.rev accum_rev) (Stream.slazy (collect_line [] width just stream))
	    | (`break as x)
	    | (`fragment _ as x)
	    | (#ops as x) ->
		(* Collect elements for current line *)
		collect_line (x::accum_rev) width just stream ()
  and justify_fix left_space right_space accum sapp =
    (* Justify with fixed space left and right *)
    let rec make_stream = function
      | [] ->
	  (* Space on the right *)
	  Stream.icons (`space right_space) sapp
      | `break::rest ->
	  (* Break is one space worth (fixed) *)
	  Stream.icons (`space 1) (make_stream rest)
      | (`fragment _ as x)::rest
      | (#ops as x)::rest ->
	  (* Passthrough for fragments and ops *)
	  Stream.icons x (make_stream rest)
    in
      (* Space on the left *)
      Stream.icons
	(`space left_space)
	(make_stream accum)
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
	  Stream.icons `newline sapp
      | `break::rest ->
	  (* Use integer arithmetic instead of float *)
	  Stream.icons
	    (`space (a / breaks))
	    (make_stream (a mod breaks + rem_space) rest)
      | (`fragment _ as x)::rest
      | (#ops as x)::rest ->
	  (* Passthrough for fragments and ops *)
	  Stream.icons x (make_stream a rest)
    in
      make_stream rem_space accum
  and justify_line width just accum sapp =
    (* Dispatch justification algorithm *)
    let break_count, frag_len = measure_line accum in
    let rem_width = max 0 (width - frag_len) in
      match just with
	| `left ->
	    justify_left rem_width accum sapp
	| `center ->
	    justify_center rem_width accum sapp
	| `right ->
	    justify_right rem_width accum sapp
	| `block ->
	    justify_block break_count rem_width accum sapp
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
      Stream.slazy (collect_line [] width just stream)
end

(*----------------------------------------------------------------------------*)

module Tabs =
struct
  type input  = [ frag | whitespaces | ops ] Stream.t
  type output = [ frag | whitespaces | ops ] Stream.t

  let stream_is_empty s = 
    Stream.peek s = None

  let streams_are_empty streams =
    List.for_all stream_is_empty streams

  let make widths_and_streams =
    let rec fold_tabs all_empty skip wts () =
      match wts with
	| [] ->
	    if all_empty then
	      Stream.sempty
	    else
	      Stream.icons
		`newline
		(Stream.slazy (fold_tabs true 0 widths_and_streams))
	| (w,t)::rest ->
	    Stream.sempty
    in
      Stream.slazy (fold_tabs true 0 widths_and_streams)
end

(*----------------------------------------------------------------------------*)

module Printer =
struct
  type input = [ frag | whitespaces | ops ] Stream.t

  let rec print ansi stream =
    prerr_string "= BEFORE =====================================================\n";
    Gc.print_stat stderr;
    begin
      try while true do
	match Stream.next stream with
	  | `fragment f ->
	      print_string ansi f
	  | #ops as o ->
	      print_ops ansi o
	  | `space n ->
	      print_space ansi n
	  | `newline ->
	      print_newline ansi ()
      done with
	  Stream.Failure -> flush ansi ()
    end;
    prerr_string "= AFTER ======================================================\n";
    Gc.full_major ();
    Gc.compact ();
    Gc.print_stat stderr
  and print_ops ansi =
    function
      | `set_intensity i ->
	  set_intensity ansi i
      | `set_underline u ->
	  set_underline ansi u
      | `set_inverted i ->
	  set_inverted ansi i
      | `set_foreground c ->
	  set_foreground ansi c
      | `set_background c ->
	  set_foreground ansi c
      | `set_context ctx ->
	  set_context ansi ctx
end
