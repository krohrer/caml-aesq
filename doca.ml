(* Kaspar Rohrer, Mon Mar 15 01:05:15 CET 2010 *)

(*----------------------------------------------------------------------------*)
(** Document Representation *)
(*----------------------------------------------------------------------------*)

type text = [`text]
type elem = [`elem|text]

type tag = [`emph | `verb | `blink | `code]
let tagstr =
  function
    | `emph -> "emph"
    | `verb -> "verb"
    | `blink -> "blink"
    | `code -> "code"

type node' =
    Text of string
  | Break
  | Seq of node' list
  | Justified of [`left] * node'
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
let justified j n = Justified (j,n)
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

module ANSI :
sig
  type linel =
      Textel of string
    | Space of int
    | OpenTag of tag
    | CloseTag
  type line = linel list

  val make_printer : ?offset:int -> ?width:int -> out_channel -> printer
end =
struct
  type linel =
      Textel of string
    | Space of int
    | OpenTag of tag
    | CloseTag

  type line = linel list

  let print_textel outc string = 
    output_string outc string

  let print_space outc len =
    for i = 0 to len-1 do
      output_string outc " "
    done

  let print_newline outc () =
    output_string outc "\n"

  let normalize ~offset ~width outc =
    function
	_ -> print_newline outc ()

  let make_printer ?(offset=0) ?(width=78) outc =
    fun document ->
      normalize ~offset ~width outc document.doc_root
end

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
      | Justified (j, n) ->
	  pp fmt n
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

