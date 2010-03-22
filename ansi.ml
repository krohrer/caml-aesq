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

type 'a stream_t = 
  | SEmpty
  | SCons of 'a * 'a stream_t lazy_t 

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
  type input = [ `linebreak | `break | `fragment of string | `ops of op list] stream_t
  type output = int * input

  let separate_lines ~width stream =
    let rec line_splitter rem_width =
      function
	| SEmpty ->
	    (* Done *)
	    SEmpty
	| SCons (c, lstream) ->
	    let stream = Lazy.force lstream in
	      match c with
		| `fragment f ->
		    (* Handle fragments separately *)
		    split_fragment f rem_width stream
		| `break ->
		    if rem_width <= 2 then
		      (* Break the line if not enough space left *)
		      SCons (`linebreak,
			     lazy (line_splitter width stream))
		    else if rem_width = width then
		      (* Ignore break at beginning of line *)
		      line_splitter width stream
		    else
		      (* Insert a break *)
		      SCons (`break,
			     lazy (line_splitter (rem_width-1) stream))
		| `linebreak ->
		    (* Passthrough linebreaks *)
		    SCons (`linebreak,
			   lazy (line_splitter width stream))
		| `ops ops ->
		    (* Passthrough for ops *)
		    SCons (`ops ops,
			   lazy (line_splitter rem_width stream))
    and split_fragment frag rem_width stream =
      let flen = String.length frag in
	if flen < rem_width then
	  (* fragment still fits on this line *)
	  SCons (`fragment frag,
		 lazy (line_splitter (rem_width - flen) stream))
	else if flen > width then
	  (* fragment must be split anyway, may as well start on this line *)
	  let left = String.slice ~last:rem_width frag in
	  let right = String.slice ~first:rem_width frag in
	    SCons (`fragment left,
		   lazy (SCons (`linebreak,
				lazy (split_fragment right width stream))))
	else
	  (* fragment fits on next line for sure *)
	  SCons (`linebreak,
		 lazy (split_fragment frag width stream))
    in
      width, line_splitter width stream
end

(*----------------------------------------------------------------------------*)

module Justification =
struct
  type input = LineSeparation.output
  type output = [ `fragment of string | `ops of op list | `linebreak ] stream_t

  let not_break =
    function
      | `break -> false
      | _ -> true

  let justify just (width,stream) =
    let rec collect_line accum =
      function
	| SEmpty ->
	    (* Justify what we have left *)
	    transform_line accum stream
	| SCons (a, lstream) ->
	    let stream = Lazy.force lstream in
	      match a with
		| `linebreak ->
		    (* Justify accumulated line *)
		    transform_line accum stream
		| x ->
		    (* Justify accumulated line *)
		    collect_line (x::accum) stream
    and transform_line accum stream =
      let stats accum =
	let rec fold break_count frag_size =
	  function
	    | [] ->
		break_count, frag_size
	    | `break::rest ->
		fold (break_count + 1) frag_size rest
	    | `fragment f::rest ->
		fold break_count (frag_size + String.length f) rest
	    | `linebreak ::_ ->
		failwith "Ansi.Justification.collect_line"
	    | `ops ops::rest ->
		fold break_count frag_size rest
	in
	  fold 0 0 accum
      in
	ignore (stats accum);
	SEmpty
    in
      collect_line [] stream
end

(*----------------------------------------------------------------------------*)

module Printer =
struct
  type input = [ `fragment of string | `ops of op list | `linebreak ] stream_t
  let rec print ansi =
    function
      | SEmpty -> ()
      | SCons (a, lstream) ->
	  let stream = Lazy.force lstream in
	    match a with
	      | `fragment f ->
		  print_string ansi f;
		  print ansi stream
	      | `ops ops ->
		  List.iter (print_ops ansi) ops;
		  print ansi stream
	      | `linebreak ->
		  print_newline ansi ();
		  print ansi stream
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
