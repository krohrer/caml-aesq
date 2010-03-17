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

module ANSIContext :
sig
  type t
end =
struct
  open Printf
  open Format

  type output =
    | OSubString of string
    | OOpenTag of tag
    | OCloseTag of tag
    | OSpace of int
    | OConc of int * output * output

  type t = {
    c_offset : int;
    c_width : int;
    mutable c_pos : int;
    mutable c_tags : tag list;
  }

  (*------------------------------------*)
  
  let make ~offset ~width = {
    c_offset = offset;
    c_width = width;
    c_pos = 0;
    c_tags = []
  }

  let splitn ctx sizes =
    let rec fold offset revctxs =
      function
	| [] -> revctxs
	| x::rest ->
	    let revctxs' = (make ~offset ~width:x)::revctxs in
	      fold (offset + x) revctxs' rest
    in
      List.rev (fold ctx.c_offset [] sizes)

  let indent ctx delta = {
    ctx with c_offset = ctx.c_offset + delta 
  }

  let tags ctx =
    ctx.c_tags

  let push_tag ctx t =
    ctx.c_tags <- t :: ctx.c_tags

  let pop_tag ctx () =
    match ctx.c_tags with
      | [] ->
	  failwith "Doca.ANSIContext.ctx_pop_tag"
      | x::rest ->
	  ctx.c_tags <- rest;
	  x

  let rem_length ctx =
    ctx.c_width - ctx.c_pos

  let pos ctx =
    ctx.c_pos

  let reset_pos ctx =
    ctx.c_pos <- 0

  (*------------------------------------*)

  let rec lines_from_node ctx =
    function
	Text t -> Stream.empty
      | Break -> Stream.empty
      | Seq ns -> Stream.empty
      | Tagged (t,n) -> Stream.empty
      | Section (title,body) -> Stream.empty
      | Table (caption,rowscols) -> Stream.empty
	      
  (*------------------------------------*)

  let ansi_tag =
    let esc s = sprintf "\x1b[%sm" s in
    let either yes no = fun f -> if f then esc yes else esc no in
    let lut = [
      `verb,  either "1" "22";
      `emph,  either "4" "24";
      `blink, either "6" "25";
      `code,  either "37" "39";
    ]
    in
      fun opens tag ->
	try (List.assoc tag lut) opens with Not_found -> either "9" "29" opens
	  
  (* let p_string ctx s = *)
  (* let lines_from_char_enum ctx pos ce = *)
  (* let rec lines ctx = *)
  (*   function *)
  (* 	Text t -> lines_from_text ctx t *)
  (*     | Break -> lines_from_break ctx () *)
  (*     | Seq ns -> lines_from_seq ctx ns *)
  (*     | Justified (j,n) -> lines_from_just ctx j n *)
  (*     | Tagged (t,n) -> lines_from_tagged ctx t n *)
  (*     | Section (title,body) -> lines_from_section ctx title body *)
  (*     | Table (caption,rowscols) -> lines_from_table ctx caption rowscols *)
  (* and lines_from_text ctx t = *)
  (*   TTextel t *)
  (* and norm_seq ctx ns = *)
  (*   TLazySeq (lazy (List.map (normalize ctx) ns)) *)
  (* and norm_just ctx j n = *)
  (*   normalize ctx n *)
  (* and norm_tagged ctx t n = *)
  (*   TLazySeq (lazy [TOpenTag (tagstr t); normalize ctx n; TCloseTag]) *)
  (* and norm_section ctx title body = *)
  (*   TLazySeq (lazy []) *)
  (* and norm_table ctx caption body = *)
  (*   TLazySeq (lazy []) *)
end

module ANSI :
sig
end =
struct
  open Printf
  open Format
  open ExtLib

  type textel =
      TTextel of string
    | TSpace of int
    | TOpenTag of tag
    | TCloseTag
    | TNewline
    | TLazySeq of textel list lazy_t

  let rec print_textel_stream fmt =
    function
	TTextel s -> pp_print_string fmt s
      | TSpace n -> for i = 0 to n-1 do pp_print_string fmt " " done
      | TOpenTag t -> pp_open_tag fmt t
      | TCloseTag -> pp_close_tag fmt ()
      | TNewline -> pp_print_newline fmt ()
      | TLazySeq ls -> List.iter (print_textel_stream fmt) (Lazy.force ls)

  let ansi_tag =
    let esc s = sprintf "\x1b[%sm" s in
    let either yes no = fun f -> if f then esc yes else esc no in
    let lut = List.map (fun (t,e) -> tagstr t, e) [
	  `verb,  either "1" "22";
	  `emph,  either "4" "24";
	  `blink, either "6" "25";
	  `code,  either "37" "39";
	]
    in
      fun opens tag ->
	try (List.assoc tag lut) opens with Not_found -> either "9" "29" opens

  let ansi_tag_functions = {
    mark_open_tag = ansi_tag true;
    mark_close_tag = ansi_tag false;
    print_open_tag = ignore;
    print_close_tag = ignore
  }

  let print_textel outc string = 
    output_string outc string

  let print_space outc len =
    for i = 0 to len-1 do
      output_string outc " "
    done

  let print_newline outc () =
    output_string outc "\n"

  type context = {
    c_offset : int;
    c_width : int
  }

  let make_context ~offset ~width = {
    c_offset = offset;
    c_width = width
  }

  let split_context ctx sizes = 
    let rec fold offset revctxs =
      function
	| [] -> revctxs
	| x::rest ->
	    let revctxs' = (make_context ~offset ~width:x)::revctxs in
	      fold (offset + x) revctxs' rest
    in
      List.rev (fold ctx.c_offset [] sizes)

  let indent_context ctx delta = {
    ctx with c_offset = ctx.c_offset + delta 
  }

  let rec normalize ctx =
    function
	Text t -> norm_text ctx t
      | Break -> TNewline
      | Seq ns -> norm_seq ctx ns
      | Tagged (t,n) -> norm_tagged ctx t n
      | Section (title,body) -> norm_section ctx title body
      | Table (caption,rowscols) -> norm_table ctx caption rowscols
  and norm_text ctx t =
    TTextel t
  and norm_seq ctx ns =
    TLazySeq (lazy (List.map (normalize ctx) ns))
  and norm_tagged ctx t n =
    TLazySeq (lazy [TOpenTag (tagstr t); normalize ctx n; TCloseTag])
  and norm_section ctx title body =
    TLazySeq (lazy [])
  and norm_table ctx caption body =
    TLazySeq (lazy [])

  let make_printer ?(offset=0) ?(width=78) outc =
    let fmt = formatter_of_out_channel outc in
      pp_set_margin fmt width;
      pp_set_tags fmt true;
      pp_set_formatter_tag_functions fmt ansi_tag_functions;
      fun document ->
	let context = make_context ~offset ~width in
	let textel_stream = normalize context document.doc_root in
	  print_textel_stream fmt textel_stream
end

(*----------------------------------------------------------------------------*)

open Printf
open Format

let ansi_tag opens tag =
  let esc s = sprintf "\x1b[%sm" s in
  let either yes no = if opens then esc yes else esc no in
    try
      List.assoc tag [
	tagstr `verb,  either "1" "22";
	tagstr `emph,  either "4" "24";
	tagstr `blink, either "6" "25";
	tagstr `code,  either "37" "39";
      ]
    with
	Not_found -> either "9" "29"

let ansi_tag_functions = {
  mark_open_tag = ansi_tag true;
  mark_close_tag = ansi_tag false;
  print_open_tag = ignore;
  print_close_tag = ignore
}

let pp_print_document fmt doc =
  let rec pp fmt =
    function 
	Text t ->
	  pp_print_string fmt t
      | Seq s ->
	  List.iter (pp fmt) s
      | Break ->
	  pp_print_string fmt "\n"
      | Tagged (t, n) ->
	  pp_open_tag fmt (tagstr t);
	  pp fmt n;
	  pp_close_tag fmt ()
      | Section (title,body) ->
	  pp_print_newline fmt ();
	  pp fmt title;
	  pp_print_newline fmt ();
	  pp fmt body
      | Table (caption,rowscols) ->
	  pp fmt caption;
	  List.iter (List.iter (pp fmt)) rowscols
  in
    pp fmt doc.doc_root

let format_printer ?(width=78) ?tag_functions fmt document =
  match tag_functions with
    | None ->
	pp_print_document fmt document
    | Some tfs ->
	(* Save state *)
	let mark_tags = pp_get_mark_tags fmt () in
	let formatter_tag_functions = pp_get_formatter_tag_functions fmt () in
	  (* Set up custom state *)
	  pp_print_newline fmt ();
	  pp_set_tags fmt true;
	  pp_set_mark_tags fmt true;
	  pp_set_formatter_tag_functions fmt tfs;
	  (* Print document *)
	  pp_print_document fmt document;
	  pp_print_newline fmt ();
	  (* Restore state *)
	  pp_set_tags fmt false;
	  pp_set_mark_tags fmt mark_tags;
	  pp_set_formatter_tag_functions fmt formatter_tag_functions

let text_printer ?width fmt document =
  format_printer ?width fmt document

let ansi_printer ?width fmt document = 
  let tag_functions = ansi_tag_functions in
    format_printer ?width ~tag_functions fmt document

