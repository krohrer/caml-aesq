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
| `set_intensity of intensity
| `set_underline of underline
| `set_inverted of bool
| `set_foreground of color
| `set_background of color
]

type format_ops = [
| `set_width of int
| `set_justification of justification
]

type context_ops = [
  `set_context of context
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

module Columns =
struct
  type input  = [ frag | whitespaces | ops ]
  type output = [ frag | whitespaces | ops ]

  let make _ = lazy SNil
end

(*----------------------------------------------------------------------------*)

module Debug =
struct
  type input = [ frag | whitespaces | breaks | ops | format_ops | context_ops ]

  open Printf

  let rec dump outc stream =
    match Lazy.force stream with
      | SNil ->
          Pervasives.flush outc
      | SCons (c, s) ->
           begin match c with
            | `nop
	    | `set_justification `left ->
		fprintf outc "\nLEFT\n"
	    | `set_justification `center ->
		fprintf outc "\nCENTER\n"
	    | `set_justification `right ->
		fprintf outc "\nRIGHT\n"
	    | `set_justification `block ->
		fprintf outc "\nBLOCK\n"
	    | `set_width w ->
		fprintf outc "\nWIDTH(%d)\n" w
	    | #ops ->
		fprintf outc "OP "
	    | #context_ops ->
                fprintf outc "CTX "
            | `fragment f ->
                fprintf outc "%S " f
            | `break ->
                fprintf outc "BR "
            | `linebreak ->
                fprintf outc "LBR\n"
            | `space n ->
                fprintf outc "S(%d) " n
            | `newline ->
                fprintf outc "NL\n"
          end;
          dump outc s
end

(*----------------------------------------------------------------------------*)

module Formatter =
struct
  type input  = [ frag | breaks | ops | format_ops ]
  type output = [ frag | whitespaces | ops ]

  let format ?(width=78) ?(just=`block) s =
    lazy SNil
end



(*----------------------------------------------------------------------------*)

module Printer =
struct
  type input = [ frag | whitespaces | breaks | ops | context_ops ]

  let rec print ansi stream =
    match Lazy.force stream with
      | SNil -> flush ansi ()
      | SCons (c, s) ->
          begin match c with
            | `fragment f ->
                print_string ansi f
            | #ops
	    | #context_ops as o ->
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
