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
(** Printing *)
(*----------------------------------------------------------------------------*)

type printer = document -> unit

let print pr doc = pr doc

(*----------------------------------------------------------------------------*)
(** ANSI Text Output *)
(*----------------------------------------------------------------------------*)

module ANSI =
struct
  type ansiop = [ `nop
  ]

  type simpleop = [
  | `frag of string
  | `ansi of ansiop
  ]

  type op = [
  | simpleop
  | `break
  | `withctx of context * op Stream.t
  ]

  and color = [
    `black | `red | `green | `yellow | `blue | `magenta | `cyan | `white | `default | `high of color
  ]

  and intensity = [
    `bold | `faint | `normal
  ]

  and context = {
    c_offset : int;
    c_width : int;
    c_depth : int;
    mutable c_intensity : intensity list;
    mutable c_underline : bool list;
    mutable c_inverted : bool list;
    mutable c_foreground : color list;
    mutable c_background : color list;
  }

  and printer = {
    mutable p_context : context;
    mutable p_column : int;
    mutable p_queue_count : int;
    p_queue : simpleop Queue.t
  }

  (*------------------------------------*)

  module Context =
  struct
    let make 
	?(offset=0) ?(width=78) ?(depth=0)
	?(intensity=`normal)
	?(underline=false)
	?(inverted=false)
	?(foreground=`default)
	?(background=`default)
	()= {
	  c_offset = offset;
	  c_width = width;
	  c_depth = depth;
	  c_intensity = [intensity];
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

    let intensity ctx  = List.tl ctx.c_intensity
    let underlines ctx = List.tl ctx.c_underline
    let inverted ctx   = List.tl ctx.c_inverted
    let foreground ctx = List.tl ctx.c_foreground
    let background ctx = List.tl ctx.c_background

    let push_intensity ctx i  = ctx.c_intensity  <- i::ctx.c_intensity
    let push_underline ctx f  = ctx.c_underline  <- f::ctx.c_underline
    let push_inverted ctx f   = ctx.c_inverted   <- f::ctx.c_inverted
    let push_foreground ctx c = ctx.c_foreground <- c::ctx.c_foreground
    let push_foreground ctx c = ctx.c_foreground <- c::ctx.c_foreground

    let pop_intensity ctx  = ctx.c_intensity  <- List.tl ctx.c_intensity
    let pop_underline ctx  = ctx.c_underline  <- List.tl ctx.c_underline
    let pop_inverted ctx   = ctx.c_inverted   <- List.tl ctx.c_inverted
    let pop_foreground ctx = ctx.c_foreground <- List.tl ctx.c_foreground
    let pop_background ctx = ctx.c_background <- List.tl ctx.c_background
  end

  (*------------------------------------*)

  module Printer =
  struct
    let make ctx = {
      p_context = ctx;
      p_column = 0;
      p_queue_count = 0;
      p_queue = Queue.create ()
    }

    let queue_frag pr (`frag s as f) =
      pr.p_queue_count <- pr.p_queue_count + (String.length s);
      Queue.add f pr.p_queue

    let queue_ansi pr (`ansi o as a) =
      Queue.add a pr.p_queue

    let flush_queue pr ctx =
      if ctx.c_depth = 0 then
	()

    let switch_context pr ctx =
      if pr.p_context != ctx then begin
	()
      end
	
    let rec print' pr ctx stream =
      let consume =
	function
	  | `frag s as f ->
	      switch_context pr ctx;
	      queue_frag pr f
	  | `ansi o as a ->
	      switch_context pr ctx;
	      queue_ansi pr a
	  | `break -> 
	      flush_queue pr ctx
	  | `withctx (ctx, stream) ->
	      print' pr ctx stream
      in
	Stream.iter consume stream
  end
end

(*----------------------------------------------------------------------------*)
