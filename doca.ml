(* Kaspar Rohrer, Mon Mar 15 01:05:15 CET 2010 *)

(*----------------------------------------------------------------------------*)
(** Document Representation *)
(*----------------------------------------------------------------------------*)

type text = [`text]
type elem = [`elem|text]

type justify = 
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
    | `justify of justify
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

module Ansi =
struct
  module Context =
  struct 
    type linelem =
      | Fragment of string
      | Space of int
      | Linebreak
      | Push_intensity of intensity
      | Push_underline of bool
      | Push_inverted of bool
      | Push_foreground of color
      | Push_background of color
      | Push_context of context
      | Pop_intensity
      | Pop_underline
      | Pop_inverted
      | Pop_foreground
      | Pop_background
      | Pop_context

    type t = {
      mutable c_intensity : intensity list;
      mutable c_underline : bool list;
      mutable c_inverted : bool list;
      mutable c_foreground : color list;
      mutable c_background : color list;
    }

   let make
	?(intensity=`normal)
	?(underline=false)
	?(inverted=false)
	?(foreground=`default)
	?(background=`default)
	lineelem list Stream = {
	  c_width = width;
	  c_depth = depth;
	  c_intensity = [intensity];
	  c_underline = [underline];
	  c_inverted = [inverted];
	  c_foreground = [foreground];
	  c_background = [background];
	}

    let child ~width ctx = {
      ctx with 
	c_width = width;
	c_depth = succ ctx.c_depth 
    }

    let width ctx = ctx.c_width
    let depth ctx = ctx.c_depth

    let intensity ctx  = List.hd ctx.c_intensity
    let underline ctx  = List.hd ctx.c_underline
    let inverted ctx   = List.hd ctx.c_inverted
    let foreground ctx = List.hd ctx.c_foreground
    let background ctx = List.hd ctx.c_background

    let push_intensity ctx i  = ctx.c_intensity  <- i::ctx.c_intensity
    let push_underline ctx f  = ctx.c_underline  <- f::ctx.c_underline
    let push_inverted ctx f   = ctx.c_inverted   <- f::ctx.c_inverted
    let push_foreground ctx c = ctx.c_foreground <- c::ctx.c_foreground
    let push_background ctx c = ctx.c_background <- c::ctx.c_background

    let pop_intensity ctx = 
      match ctx.c_intensity with
	| [] -> failwith "ANSI.Context.pop_intensity"
	| x::rest -> ctx.c_intensity <- rest; x
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

    let make outc ctx = {
      p_outc = outc;
      p_context = ctx;
      p_column = 0;
      p_queue_fragments = 0;
      p_queue_columns = 0;
      p_queue = Queue.create ()
    }

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

(*----------------------------------------------------------------------------*)
