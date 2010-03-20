(* Kaspar Rohrer, Mon Mar 15 01:05:15 CET 2010 *)

(*----------------------------------------------------------------------------*)
(** Document Representation *)
(*----------------------------------------------------------------------------*)

type text = [`text]
type elem = [`elem|text]

type justification = 
    [ `left
    | `center 
    | `right 
    | `block
    ]

type tag =
    [ `emph
    | `verb 
    | `blink
    | `code
    | `justify of justification
    ]

let tagstr =
  function
    | `emph -> "emph"
    | `verb -> "verb"
    | `blink -> "blink"
    | `code -> "code"
    | `justify j -> "blink"

type node' =
    Text of string
  | Break
  | Seq of node' list
  | Tagged of tag * node'
  | Section of node' * node'
  | Table of node' * node' list list

type 't node = node'

type document = { 
  doc_author : string option;
  doc_header : (?page:int*int -> unit -> node') option;
  doc_footer : (?page:int*int -> unit -> node') option;
  doc_root : node'
}

let document ?author ?header ?footer nodes = {
  doc_author = author;
  doc_header = header;
  doc_footer = footer;
  doc_root = Seq nodes
}

let text s = Text s
let break = Break
let tagged t n = Tagged (t, n)
let justified j n = Tagged (`justify j,n)
let emph n = tagged `emph n
let verb n = tagged `verb n
let blink n = tagged `blink n
let code n = tagged `code n
let left n = justified `left n

let section title body = Section (title, Seq body)
let table caption rowscols = Table (caption, rowscols)

let (~~) s = text s

(*----------------------------------------------------------------------------*)
(** Localization *)
(*----------------------------------------------------------------------------*)

type 'l lang = 'l

let language l = l
let localize l lns = List.assoc l lns

(*----------------------------------------------------------------------------*)
(** ANSI Text Output *)
(*----------------------------------------------------------------------------*)

module ANSI =
struct
  type color = [`black|`red|`green|`yellow|`blue|`magenta|`cyan|`white|`default]

  type intensity = [`bold|`faint|`normal]

  type justify = [`left|`center|`right|`block]

  type elem = [
  | `frag of string
  | `intensity of intensity
  | `justify of justify
  | `underline of bool
  | `inverted of bool
  | `foreground of color
  | `background of color
  ]

  (*------------------------------------*)

  module Context =
  struct 
    type t = {
      c_offset : int;
      c_width : int;
      c_depth : int;
      mutable c_intensity : intensity list;
      mutable c_justify : justify list;
      mutable c_underline : bool list;
      mutable c_inverted : bool list;
      mutable c_foreground : color list;
      mutable c_background : color list;
    }

   let make 
	?(offset=0) ?(width=78) ?(depth=0)
	?(intensity=`normal)
	?(justify=`left)
	?(underline=false)
	?(inverted=false)
	?(foreground=`default)
	?(background=`default)
	()= {
	  c_offset = offset;
	  c_width = width;
	  c_depth = depth;
	  c_intensity = [intensity];
	  c_justify = [justify];
	  c_underline = [underline];
	  c_inverted = [inverted];
	  c_foreground = [foreground];
	  c_background = [background];
	}

    let child ~offset ~width ctx = {
      ctx with 
	c_offset = offset;
	c_width = width;
	c_depth = succ ctx.c_depth 
    }

    let offset ctx = ctx.c_offset
    let width ctx = ctx.c_width
    let depth ctx = ctx.c_depth

    let intensity ctx  = List.hd ctx.c_intensity
    let justify ctx    = List.hd ctx.c_justify
    let underline ctx  = List.hd ctx.c_underline
    let inverted ctx   = List.hd ctx.c_inverted
    let foreground ctx = List.hd ctx.c_foreground
    let background ctx = List.hd ctx.c_background

    let push_intensity ctx i  = ctx.c_intensity  <- i::ctx.c_intensity
    let push_justify ctx j    = ctx.c_justify    <- j::ctx.c_justify
    let push_underline ctx f  = ctx.c_underline  <- f::ctx.c_underline
    let push_inverted ctx f   = ctx.c_inverted   <- f::ctx.c_inverted
    let push_foreground ctx c = ctx.c_foreground <- c::ctx.c_foreground
    let push_background ctx c = ctx.c_background <- c::ctx.c_background

    let pop_intensity ctx = 
      match ctx.c_intensity with
	| [] -> failwith "ANSI.Context.pop_intensity"
	| x::rest -> ctx.c_intensity <- rest; x
    let pop_justify ctx =
      match ctx.c_justify with
	| [] -> failwith "ANSI.Context.pop_justify"
	| x::rest -> ctx.c_justify <- rest; x
    let pop_underline ctx =
      match ctx.c_underline with
	| [] -> failwith "ANSI.Context.pop_underline"
	| x::rest -> ctx.c_underline <- rest; x
    let pop_inverted ctx =
      match ctx.c_inverted with
	| [] -> failwith "ANSI.Context.pop_inverted"
	| x::rest -> ctx.c_inverted <- rest; x
    let pop_foreground ctx =
      match ctx.c_foreground with
	| [] -> failwith "ANSI.Context.pop_foreground"
	| x::rest -> ctx.c_foreground <- rest; x
    let pop_background ctx =
      match ctx.c_background with
	| [] -> failwith "ANSI.Context.pop_background"
	| x::rest -> ctx.c_background <- rest; x
  end

  (*------------------------------------*)

  module Printer =
  struct
    module C = Context

    type t = {
      p_outc : out_channel;
      mutable p_context : Context.t;
      mutable p_column : int;
      mutable p_queue_columns : int;
      mutable p_queue_fragments : int;
      p_queue : elem Queue.t
    }

    type op = [
    | `opcode of (t -> unit)
    | `frag of string
    | `break
    | `pushctx of Context.t
    | `popctx
    ]

    let make outc ctx = {
      p_outc = outc;
      p_context = ctx;
      p_column = 0;
      p_queue_fragments = 0;
      p_queue_columns = 0;
      p_queue = Queue.create ()
    }

    let print_string pr s =
      output_string pr.p_outc s

    let queue_add_fragment pr (`frag s as f) =
      pr.p_queue_fragments <- pr.p_queue_fragments + 1;
      pr.p_queue_columns <- pr.p_queue_columns + (String.length s);
      Queue.add f pr.p_queue
    let queue_add pr elem =
      Queue.add elem pr.p_queue

    let flush_queue pr ctx =
      (* TODO *)
      if C.depth ctx = 0 then
	output_string pr.p_outc "\n"
 
    let print_esc pr code =
      Printf.fprintf pr.p_outc "\x1b[%dm" code

    let print_intensity pr =
      function
	| `bold   -> print_esc pr 1 
	| `normal -> print_esc pr 22
	| `faint  -> print_esc pr 2

    let print_underline pr =
      function
	| true -> print_esc pr 4
	| false -> print_esc pr 24

    let print_inverted pr =
      function
	| true -> print_esc pr 7
	| false -> print_esc pr 27

    let print_foreground pr =
      function
	| `black ->
	    print_esc pr 30
	| `red ->
	    print_esc pr 31
	| `green ->
	    print_esc pr 32
	| `yellow -> 
	    print_esc pr 33
	| `blue -> 
	    print_esc pr 34
	| `magenta ->
	    print_esc pr 35
	| `cyan ->
	    print_esc pr 36
	| `white ->
	    print_esc pr 37
	| `default ->
	    print_esc pr 39

    let print_background pr =
      function
	| `black ->
	    print_esc pr 40
	| `red ->
	    print_esc pr 41
	| `green ->
	    print_esc pr 42
	| `yellow -> 
	    print_esc pr 43
	| `blue -> 
	    print_esc pr 44
	| `magenta ->
	    print_esc pr 45
	| `cyan ->
	    print_esc pr 46
	| `white ->
	    print_esc pr 47
	| `default ->
	    print_esc pr 49

    let print_ansi pr =
      function
	| `intensity i -> 
	    print_intensity pr i
	| `underline f ->
	    print_underline pr f
	| `inverted f ->
	    print_inverted pr f
	| `foreground c ->
	    print_foreground pr c
	| `background c ->
	    print_background pr c

    let switch_context pr ctx =
      let prctx = pr.p_context in
	if prctx != ctx then begin
	  begin let i = C.intensity ctx in
	    if i <> C.intensity prctx then
	      print_intensity pr i
	  end;
	  begin let f = C.underline ctx in
	    if f <> C.underline prctx then
	      print_underline pr f
	  end;
	  begin let f = C.inverted ctx in
	    if f <> C.inverted prctx then
	      print_inverted pr f
	  end;
	  begin let c = C.foreground ctx in
	    if c <> C.foreground prctx then
	      print_foreground pr c
	  end;
	  begin let c = C.background ctx in
	    if c <> C.background prctx then
	      print_background pr c
	  end
	end

    let rec print pr ctx stream =
      let consume =
	function
	  | `frag _ as f ->
	      queue_add_fragment pr f
	  | `push_intensity i ->
	      let i' = C.intensity ctx in
		if i <> i' then
		  queue_add pr (`intensity i);
		C.push_intensity ctx i
	  | `push_justify j ->
	      let j' = C.justify ctx in
		if j <> j' then
		  queue_add pr (`justify j);
		C.push_justify ctx j
	  | `push_underline f ->
	      let f' = C.underline ctx in
		if f <> f' then
		  queue_add pr (`underline true);
		C.push_underline ctx f
	  | `push_inverted f   ->
	      let f' = C.inverted ctx in
		if f <> f' then
		  queue_add pr (`inverted true);
		C.push_underline ctx f
	  | `push_foreground c ->
	      let c' = C.foreground ctx in
		if c <> c' then 
		  queue_add pr (`foreground c);
		C.push_foreground ctx c
	  | `push_background c ->
	      let c' = C.background ctx in
		if c <> c' then queue_add pr (`background c);
		C.push_background ctx c
	  | `pop_intensity ->
	      let i' = C.pop_intensity ctx in
	      let i = C.intensity ctx in
		if i <> i' then 
		  queue_add pr (`intensity i)
	  | `pop_underline ->
	      let f' = C.pop_underline ctx in
	      let f = C.underline ctx in
		if f <> f' then 
		  queue_add pr (`underline f)
	  | `pop_inverted ->
	      let f' = C.pop_inverted ctx in
	      let f = C.inverted ctx in
		if f <> f' then 
		  queue_add pr (`inverted f)
	  | `pop_foreground ->
	      let c' = C.pop_foreground ctx in
	      let c = C.foreground ctx in
		if c <> c' then 
		  queue_add pr (`foreground c)
	  | `pop_background ->
	      let c' = C.pop_foreground ctx in
	      let c = C.foreground ctx in
		if c <> c' then 
		  queue_add pr (`background c)
	  | `break -> 
	      flush_queue pr ctx
	  | `withctx (ctx', stream) ->
	      switch_context pr ctx';
	      print pr ctx' stream;
	      switch_context pr ctx 
      in
	Stream.iter consume stream
  end

  module Format =
  struct
    (* TODO : Stream creation from nodes*)
    let format ctx node = Stream.sempty
  end

  let print ?offset ?width outc doc =
    let ctx = Context.make ?offset ?width () in
    let pr = Printer.make outc ctx in
    let opstream = Format.format ctx doc.doc_root in
      Printer.print pr ctx opstream
end

let ansi_print = ANSI.print

(*----------------------------------------------------------------------------*)
