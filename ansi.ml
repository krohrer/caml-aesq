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
  let ctx = p.p_active in
    ctx.c_intensity <- `normal;
    ctx.c_underline <- `none;
    ctx.c_inverted <- false;
    ctx.c_foreground <- `default;
    ctx.c_background <- `default

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
  let i = p.p_real.c_intensity in
  let active = p.p_active in
    if i <> active.c_intensity then begin
      active.c_intensity <- i;
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
  let u = p.p_real.c_underline in
  let active = p.p_active in
    if u <> active.c_underline then begin
      active.c_underline <- u;
      let c =
	match u with
	  | `single -> 4 
	  | `none -> 24
      in
	c :: codes
    end else
      codes

let enforce_inverted p codes =
  let i = p.p_real.c_inverted in
  let active = p.p_active in
    if i <> active.c_inverted then begin
      active.c_inverted <- i;
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
  let c = p.p_real.c_foreground in
  let active = p.p_active in
    if c <> active.c_foreground then begin
      active.c_foreground <- c;
      (color_code 30 c) :: codes
    end else
      codes

let enforce_background p codes =
  let c = p.p_real.c_background in
  let active = p.p_active in
    if c <> active.c_background then begin
      active.c_background <- c;
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

type op = [ 
| `set_intensity of intensity
| `set_underline of underline
| `set_inverted of bool
| `set_foreground of color
| `set_background of color
| `set_context of context
]

(*----------------------------------------------------------------------------*)

module LineSeparation =
struct
  type input = [ `linebreak | `break | `fragment of string | `ops of op list] Stream.t
  type output = int * input

  let separate_lines ~width stream =
    let rec splitter rem_width () =
      try match Stream.next stream with
	| `fragment f ->
	    (* Handle fragments separately *)
	    split_fragment f rem_width ()
	| `break ->
	    accumulate_breaks rem_width ()
	| (`linebreak as x) ->
	    Stream.icons x (Stream.slazy (splitter width))
	| (`ops _ as x) ->
	    Stream.icons x (Stream.slazy (splitter rem_width))
	      (* Passthrough linebreaks and ops *)
      with
	  Stream.Failure -> Stream.sempty
    and accumulate_breaks rem_width () =
      try match Stream.next stream with
	| `fragment f ->
	    if rem_width <= 2 then
	      (* Break the line if not enough space left *)
	      Stream.icons `linebreak (Stream.slazy (split_fragment f width))
	    else if rem_width = width then
	      (* Ignore break at beginning of line *)
	      split_fragment f rem_width ()
	    else
	      (* Insert a break *)
	      Stream.icons `break (Stream.slazy (split_fragment f (rem_width-1)))
	| `break ->
	    (* Just another break in the wall (stream) *)
	    accumulate_breaks rem_width ()
	| (`linebreak as x) ->
	    (* Ignore breaks and start new line*)
	    Stream.icons x (Stream.slazy (splitter width))
	| (`ops _ as x) ->
	    (* Pass-through ops *)
	    Stream.icons x (Stream.slazy (accumulate_breaks rem_width))
      with
	  Stream.Failure -> Stream.sempty
    and split_fragment frag rem_width () =
      let flen = String.length frag in
	if flen < rem_width then
	  (* fragment still fits on this line *)
	  Stream.icons (`fragment frag) (Stream.slazy (splitter (rem_width - flen)))
	else if flen > width then
	  (* fragment must be split anyway, may as well start on this line *)
	  let left = String.slice ~last:rem_width frag in
	  let right = String.slice ~first:rem_width frag in
	    Stream.icons
	      (`fragment left)
	      (Stream.icons
		 `linebreak
		 (Stream.slazy (split_fragment right width)))
	else
	  (* fragment fits on next line for sure *)
	  Stream.icons
	    `linebreak
	    (Stream.slazy (split_fragment frag width))
    in
      width, splitter width ()
end

(*----------------------------------------------------------------------------*)

module Justification =
struct
  type input = LineSeparation.output
  type output = [ `fragment of string | `ops of op list | `linebreak | `space of int] Stream.t

  let justify just (width,stream) =
    let rec collect_line accum_rev () =
      (* Collect non-linebreak elements for later justification *)
      try match Stream.next stream with
	| `linebreak ->
	    (* Justify accumulated line *)
	    justify_line (List.rev accum_rev)
	| (`break as x)
	| (`fragment _ as x)
	| (`ops _ as x) ->
	    (* Collect elements for current line *)
	    collect_line (x::accum_rev) ()
      with
	  Stream.Failure -> justify_line (List.rev accum_rev)
    and justify_fix left_space right_space accum =
      (* Justify with fixed space left and right *)
      let rec make_stream = function
	| [] ->
	    (* Space on the right *)
	    Stream.icons
	      (`space right_space)
	      (Stream.slazy (collect_line []))
	| `break::rest ->
	    (* Break is one space worth (fixed) *)
	    Stream.icons (`space 1) (make_stream rest)
	| (`fragment _ as x)::rest
	| (`ops _ as x)::rest ->
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
    and justify_block breaks rem_space accum =
      (* Justify space evenly for all breaks *)
      let rec make_stream a = function
	| [] ->
	    collect_line [] ()
	| `break::rest ->
	    (* Use integer arithmetic instead of float *)
	    Stream.icons
	      (`space (a / breaks))
	      (make_stream (a mod breaks + rem_space) rest)
	| (`fragment _ as x)::rest
	| (`ops _ as x)::rest ->
	    (* Passthrough for fragments and ops *)
	    Stream.icons x (make_stream a rest)
      in
	make_stream rem_space accum
    and justify_line accum =
      (* Dispatch justification algorithm *)
      let break_count, frag_len = measure_line accum in
      let rem_width = max 0 (width - frag_len) in
	match just with
	  | `left ->
	      justify_left rem_width accum
	  | `center ->
	      justify_center rem_width accum
	  | `right ->
	      justify_right rem_width accum
	  | `block ->
	      justify_block break_count rem_width accum
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
	  | `ops ops::rest ->
	      fold break_count frag_len rest
      in
	fold 0 0 accum
    in
      collect_line [] ()
end

(*----------------------------------------------------------------------------*)

module Printer =
struct
  type input = [ `fragment of string | `ops of op list | `space of int | `linebreak ] Stream.t
  let rec print ansi stream =
    try match Stream.next stream with
      | `fragment f ->
	  print_string ansi f;
	  print ansi stream
      | `ops ops ->
	  List.iter (print_ops ansi) ops;
	  print ansi stream
      | `space n ->
	  print_space ansi n;
	  print ansi stream
      | `linebreak ->
	  print_newline ansi ();
	  print ansi stream
    with
	Stream.Failure -> ()
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
