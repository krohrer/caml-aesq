(* Kaspar Rohrer, Thu Apr  8 02:24:17 CEST 2010 *)

open Format

let rec obj_is_list r =
  if Obj.is_int r then
    r = Obj.repr 0
  else
    if Obj.tag r = 0 && Obj.size r = 2 then
      obj_is_list (Obj.field r 1)
    else
      false

let rec dump fmt r =
  ()

and dump_int fmt r =
  pp_print_int fmt (Obj.magic r)

and dump_opaque fmt str r =
  pp_open_box fmt 2;
  pp_print_string fmt "<";
  pp_print_string fmt str;
  for i = 0 to Obj.size r do
    if i > 0 then
      pp_print_space fmt ();
    dump fmt (Obj.field r i)
  done;
  pp_print_string fmt ">";
  pp_close_box fmt ()

and dump_list fmt r =
  if Obj.is_int r then
    assert (Obj.magic r = 0)
  else begin
    assert (Obj.tag r = 0 && Obj.size r = 2);
    let head = Obj.field r 0 and tail = Obj.field r 1 in
      dump fmt head;
      if Obj.is_int tail then
	assert (Obj.magic r = 0)
      else begin
	pp_print_string fmt ";";
	pp_print_space fmt ();
	dump_list fmt tail
      end
  end

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
    Stack.push (Obj.repr o) candidates;
    while has_candidates () do
      let r = next_candidate () in
      let t = Obj.tag r in
	if t < Obj.no_scan_tag then begin
	  let n = Obj.size r in
      	    for i = 0 to n do add_candidate (Obj.field r i) done;
	    add_header ();
	    add_words n
	end else if
	  t = Obj.abstract_tag ||
	  t = Obj.string_tag ||
	  t = Obj.double_tag ||
	  t = Obj.double_array_tag ||
	  t = Obj.custom_tag
	then begin
	  add_header ();
	  add_words (Obj.size r)
	end else
	  ()
    done;
    !bytes
