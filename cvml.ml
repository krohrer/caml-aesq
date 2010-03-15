(* Kaspar Rohrer, Sun Mar 14 17:54:19 CET 2010 *)

(*----------------------------------------------------------------------------*)

module type S = 
sig
  type document
  type 'l dnode

  exception Localization of string

  val document : ?lang:([>`en] as 'l) -> 'l dnode -> document

  val text : ?lang:([>`en] as 'l) -> string -> 'l dnode
  val nil : 'l dnode

  val en : string -> [`en] dnode
  val de : string -> [`de] dnode

  val footnote : 'l dnode -> 'l dnode
  val header : (?page:int*int -> unit -> 'l dnode) -> 'l dnode
  val footer : (?page:int*int -> unit -> 'l dnode) -> 'l dnode

  val section : 'l dnode -> 'l dnode -> 'l dnode
  val table : 'l dnode -> 'l dnode array array -> 'l dnode
  val olist : 'l dnode list -> 'l dnode
  val ilist : 'l dnode list -> 'l dnode

  val (~~) : string -> 'l dnode
  val (|||) : 'l dnode -> 'l dnode -> 'l dnode
end

(*----------------------------------------------------------------------------*)

module PrettyPrint : S =
struct
  open Format
  open ExtLib

  type 'l document = {
    d_author : string option;
    d_root : 'l dnode
  }

  type 'l dnode =
      Text of 'l text_t
    | Nil
    | Footnote of 'l footnote_t
    | Header of 'l header_t
    | Footer of 'l footer_t
    | Section of 'l section_t
    | Table of 'l table_t
    | UL of 'l ul_t
    | OL of 'l ol_t
  and 'l text_t = 'l * string list
  and 'l footnote_t = 'l dnode
  and 'l header_t = ?page:int*int -> unit -> 'l dnode
  and 'l footer_t = ?page:int*int -> unit -> 'l dnode
  and 'l section_t = 'l dnode * 'l dnode
  and 'l table_t = 'l dnode * 'l dnode list list
  and 'l ul_t = 'l dnode list
  and 'l ol_t = 'l dnode list

  exception Localization of string

  let default opt d =
    match opt with Some v -> v | None -> d

  let rec fold_top
    ?ftext
    ?ffootnote
    ?fheader
    ?ffooter
    ?fsection
    ?ftable
    ?ful
    ?fol
    a
    n
      =
    in
    let rec fold a =
      function
	| Nil -> a
	| Text t -> ftext' a t
	| Footnote f -> ffootnote' a f
	| Header h -> fheader' a h
	| Footer f -> ffooter' a f
	| Section s -> fsection' a s
	| Table t -> ftable' a t
	| UL l -> ful a l
	| OL l -> fol a l
    and ftext' =
      default ftext (fun a _ -> a)
    and ffootnote' =
      default ffootnote (fun a f -> fold a f)
    and fheader' =
      default fheader (fun a h -> f a (h ()))
    and ffooter' =
      default ffooter (fun a f -> f a (f ()))
    and fsection' =
      default fsection (fun a (t,s) -> flist fold (fold a t) s)
    and ftable' =
      default ftable (fun a rc -> List.fold_left flist a rc)
    and ful' =
      default ful (fun a l -> flist a l)
    and fol' = 
      default fol (fun a l -> flist a l)
    and flist a l =
      List.fold_left fold a l
    in
      fold a n
    
  (*------------------------------------*)

  type 'l context = {
    mutable c_header : 'l dnode option;
    mutable c_footer : 'l dnode option;
    mutable c_footnote_count : int;
    mutable c_footnotes_rev : 'l dnode list
  }

  let make_context () = {
    c_header = None;
    c_footer = None;
    c_footnote_count = 0;
    c_footnotes_rev = []
  }

  let add_footnote ctx fn = {
    ctx with
      c_footnote_count = ctx.c_footnote_count + 1;
      c_footnotes_rev = fn :: ctx.c_footnotes_rev
  }

  let set_header ctx h = {
    ctx with c_header = h
  }

  let set_footer ctx h = {
    ctx with c_footer = h
  }

  (*------------------------------------*)

  let gather n =

  let fold_top
      ?text:('a -> 'l text_t -> 'a)
      ?footnote:('a -> 'l footnote_t -> 'a)
      ?header:('a -> 'l header_t -> 'a)
      ?footer:('a -> 'l footer_t -> 'a)
      ?section:('a -> 'l section_t -> 'a)
      ?table:('a -> 'l table_t -> 'a)
      ?ul:('a -> 'l ul_t -> 'a)
      ?ol:('a -> 'l ol_t -> 'a)
      (a:'a) =
    function
	Text t -> text a t
      | Nil -> a
      | Footnote f -> footnote a f
      | Header h -> header a h
      | Footer f -> footer a f
      | Section s -> section a s
      | Table t -> table a t
      | UL l -> ul a l
      | OL l -> ol a l
    

  let rec print lang fmt { d_author, d_root } =
    ()
  and pp ctx lang fmt = begin function
      Text text ->
	pp_text ctx lang fmt text
    | Nil ->
	()
    | Footnote footnote ->
	pp_footnote ctx lang fmt footnote
    | Header header ->
	pp_header ctx lang fmt header
    | Footer footer ->
	pp_footer ctx lang fmt footer
    | Section section ->
	pp_section ctx lang fmt section
    | Table table ->
	pp_table ctx lang fmt table
    | UL ul ->
	pp_ul ctx lang fmt ul
    | OL ol ->
	pp_ol ctx lang fmt ol
  end
  and pp_text ctx lang fmt (l,s) =
    if lang = l then
      String.iter (print_char fmt) s
    else
      raise (Localization s)
  and pp_footnote ctx lang fmt _ =
    add_footnote ctx 
    ()
  and pp_header ctx lang fmt _ =
    ()
  and pp_footer ctx lang fmt _ =
    ()
  and pp_section ctx lang fmt _ =
    ()
  and pp_table ctx lang fmt _ =
    ()
  and pp_ul ctx lang fmt _ =
    ()
  and pp_ol ctx lang fmt _ =
    ()
  and pp_char fmt c =
    match c with
	' ' | '\t' | '\n' -> pp_print_space fmt ()
      | c -> pp_print_char fmt c

  let text ?(lang=`en) s =
    Text (`en,s)

  and nil l fmt =
    Nil
  and

  let en s = text ~lang:`en s
end


