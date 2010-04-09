(* Kaspar Rohrer, Thu Apr  8 02:24:17 CEST 2010 *)

open Format
open Obj

let failfmt fmt =
  let k s =
    failwith ("Inspect: " ^ s)
  in
    Printf.ksprintf k fmt

let rec obj_is_list r =
  if is_int r then
    r = repr 0
  else
    if tag r = 0 && size r = 2 then
      obj_is_list (field r 1)
    else
      false

let rec dump fmt r =
  let dump_int flag fmt i = 
    match flag with
      | `hex -> fprintf fmt "0x%X" i
      | `dec -> pp_print_int fmt i
  in
    match tag r with
      | x when x = lazy_tag ->
	  ()
      | x when x = closure_tag ->
	  ()
      | x when x = object_tag ->
	  ()
      | x when x = infix_tag ->
	  ()
      | x when x = forward_tag ->
	  ()
      | x when x < no_scan_tag ->
	  ()
      | x when x = abstract_tag ->
	  ()
      | x when x = string_tag ->
	  ()
      | x when x = double_tag ->
	  ()
      | x when x = double_array_tag ->
	  ()
      | x when x = custom_tag ->
	  ()
	    (* | x when x = final_tag -> () (* Same as custom_tag *) *)
      | x when x = int_tag ->
	  dump_int `dec fmt (magic r)
      | x when x = out_of_heap_tag ->
	  dump_int `hex fmt (magic r)
      | x when x = unaligned_tag ->
	  dump_int `hex fmt (magic r)
	    (* according to ${OCAMLSRC}/byterun/mlvalues.h *)
      | x -> failfmt "OCaml value with unknown tag = %d" x

(*----------------------------------------------------------------------------*)

let wobytes = Sys.word_size / 8
let hdbytes = wobytes

module HT = Hashtbl.Make(
  struct
    type t = Obj.t
    let equal = (==)
    let hash = Hashtbl.hash
  end
)

open ExtLib

let heap_size o =
  let bytes = ref 0 in
  let add_bytes d =
    bytes := !bytes + d
  in
  let add_words n =
    add_bytes (n * wobytes)
  in
  let add_header () =
    add_bytes hdbytes
  in
  let inspected = HT.create 31337 in
  let candidates = Stack.create () in
  let add_candidate r =
    if HT.mem inspected r then
      ()
    else
      Stack.push r candidates
  in
  let has_candidates () = 
    not (Stack.is_empty candidates)
  in
  let next_candidate () =
    let r = Stack.pop candidates in
      HT.add inspected r ();
      r
  in
    Stack.push (repr o) candidates;
    while has_candidates () do
      let r = next_candidate () in
      let t = tag r in
	if t < no_scan_tag then begin
	  let n = size r in
      	    for i = 0 to n do add_candidate (field r i) done;
	    add_header ();
	    add_words n
	end else if
	  t = abstract_tag ||
	  t = string_tag ||
	  t = double_tag ||
	  t = double_array_tag ||
	  t = custom_tag
	then begin
	  add_header ();
	  add_words (size r)
	end else
	  ()
    done;
    !bytes
