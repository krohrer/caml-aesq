(* Kaspar Rohrer, Sat Apr  3 16:58:26 CEST 2010 *)

open ExtLib
open Printf

let failfmt fmt =
  let k s = failwith ("Hyphenate: " ^ s) in
  ksprintf k fmt

type t = {
  h_patterns : trie;
  h_exceptions : (string, points) Hashtbl.t;
}

and trie = {
  mutable t_points : points option;
  t_children : (char, trie) Hashtbl.t;
}

and points = int array

let make_trie () =
  {
    t_points = None;
    t_children = Hashtbl.create 0;
  }

let trie_insert trie str points =
  let n = String.length str in
  let t = ref trie in
  let i = ref 0 in
    while !i < n do
      let c = str.[!i] and trie = !t in
	begin try
	  t := Hashtbl.find trie.t_children c
	with
	    Not_found ->
	      t := make_trie ();
	      Hashtbl.add trie.t_children c !t
	end;
	i := !i + 1
    done;
    !t.t_points <- Some points

let make_pattern_trie patterns =
  let trie = make_trie () in
  let add pat =
    ()
  in
    Array.iter add patterns;
    trie

let chars_and_points_from_exception ubuf pbuf utf8 =
  let aux uc =
    match UChar.code uc with
      | 0x2D
      | 0x2010 ->
	  if DynArray.length pbuf <= 1 then
	    failfmt "Malformed hyphenation exception: %s" utf8;
	  DynArray.delete_last pbuf;
	  DynArray.add pbuf 1
      | _ ->
	  UTF8.Buf.add_char ubuf uc;
	  DynArray.add pbuf 0
  in
    UTF8.Buf.clear ubuf;
    DynArray.clear pbuf;
    DynArray.add pbuf 0;
    UTF8.iter aux utf8;
    ( UTF8.Buf.contents ubuf,
      DynArray.to_array pbuf )

let chars_and_points_from_pattern ubuf pbuf utf8 =
  let aux uc =
    match UChar.code uc with
	(* [0-9] *)
      | 0x30 | 0x31 | 0x32 | 0x33 | 0x34
      | 0x35 | 0x36 | 0x37 | 0x38 | 0x39 as c ->
	  if DynArray.length pbuf = 0 then
	    failfmt "Malformed hyphenation pattern: %s" utf8;
	  DynArray.delete_last pbuf;
	  DynArray.add pbuf (c - 0x30)
      | _ ->
	  UTF8.Buf.add_char ubuf uc;
	  DynArray.add pbuf 0
  in
    UTF8.Buf.clear ubuf;
    DynArray.clear pbuf;
    UTF8.iter aux utf8;
    ( UTF8.Buf.contents ubuf,
      DynArray.to_array pbuf ) 

let make_exception_lookup exceptions =
  let lookup = Hashtbl.create 13 in
  let pbuf = DynArray.make 20 in
  let ubuf = UTF8.Buf.create 20 in
  let add utf8 =
    let chars, points = chars_and_points_from_exception ubuf pbuf utf8 in
      Hashtbl.add lookup chars points
  in
    Array.iter add exceptions;
    lookup

let make patterns exceptions =
  {
    h_patterns = make_pattern_trie patterns;
    h_exceptions = make_exception_lookup exceptions; 
  }
