(* Kaspar Rohrer, Thu Apr  8 02:24:17 CEST 2010 *)

open Format
open Obj

module ValueRepr =
struct
  type t = Obj.t
  let equal = (==)
  let hash = Hashtbl.hash
end

module HT = Hashtbl.Make(ValueRepr)

let failfmt fmt =
  let k s =
    failwith ("Inspect: " ^ s)
  in
    Printf.ksprintf k fmt

let addr r = magic r lsl 1
  (* according to ${OCAMLSRC}/byterun/mlvalues.h, integers always have
     the LSB set, and only the upper (Sys.word_size - 1) bits are used
     to represent the number. Which means that the actual number is
     the bits of the value right-shifted by 1. If we want to print the
     bit-pattern (or address) of the value, we therefore have to
     left-shift by 1. *)

let dump_with_formatter fmt o =
  let dumped_blocks = HT.create 31337 in
  let indentation_for_string id = 2 (* String.length id + 2 *) in

  let rec dump_int fmt i = 
    pp_print_int fmt i

  and dump_out_of_heap fmt a =
    fprintf fmt "0x%X!" a

  and dump_unaligned fmt a =
    fprintf fmt "0x%X?" a

  and dump_double fmt d =
    fprintf fmt "%e" d

  and dump_block name fmt r =
    try
      fprintf fmt "@@%s" (HT.find dumped_blocks r)
    with Not_found -> begin
      let id = sprintf "%s/%d" name (HT.length dumped_blocks) in
	HT.add dumped_blocks r id;
	fprintf fmt "@[<b %d>(%s" (indentation_for_string id) id;
	for i = 0 to size r - 1 do
	  fprintf fmt "@ ";
	  let f = field r i in
	    dump_aux fmt f
	done;
	fprintf fmt ")@]@,"
    end

  and dump_double_array a fmt r =
    try
      fprintf fmt "@@%s" (HT.find dumped_blocks r)
    with Not_found -> begin
      let id = sprintf "FLA/%d" (HT.length dumped_blocks) in
	HT.add dumped_blocks r id;
	fprintf fmt "@[<b %d>(%s" (indentation_for_string id) id;
	for i = 0 to Array.length a - 1 do
	  fprintf fmt "@ ";
	  dump_double fmt a.(i)
	done;
	fprintf fmt ")@]@,"
    end
      
  and dump_abstract fmt r =
    fprintf fmt "ABST+%X" (addr r)

  and dump_custom fmt r =
    (* According to ${OCAMLSRC}/byterun/custom.h, the first
       field is a pointer to a c-string that identifies the
       custom value. As we cannot directly print strings, we
       print the address of the identifier instead. *)
    fprintf fmt "CUST+%X" (addr (field r 0))

  and dump_string fmt s =
    fprintf fmt "\"%s\"" s

  and dump_aux fmt r =
    match tag r with
      | x when x = lazy_tag ->
	  dump_block "LZY" fmt r
      | x when x = closure_tag ->
	  dump_block "CLO" fmt r
      | x when x = object_tag ->
	  dump_block "OBJ" fmt r
      | x when x = infix_tag ->
	  dump_block "IFX" fmt r
      | x when x = forward_tag ->
	  dump_block "FWD" fmt r
      | x when x < no_scan_tag ->
	  dump_block (sprintf "T%d" x) fmt r
      | x when x = abstract_tag ->
	  dump_abstract fmt r
      | x when x = string_tag ->
	  dump_string fmt (magic r)
      | x when x = double_tag ->
	  dump_double fmt (magic r)
      | x when x = double_array_tag ->
	  dump_double_array (magic r) fmt r
      | x when x = custom_tag ->
	  dump_custom fmt r
	    (* | x when x = final_tag -> () (* Same as custom_tag *) *)
      | x when x = int_tag ->
	  dump_int fmt (magic r)
      | x when x = out_of_heap_tag ->
	  dump_out_of_heap fmt (addr r)
      | x when x = unaligned_tag ->
	  dump_unaligned fmt (addr r)
      | x ->
	  failfmt "OCaml value with unknown tag = %d" x
  in
    dump_aux fmt (repr o);
    pp_print_newline fmt ()

(*------------------------------------*)

let dump o =
  dump_with_formatter std_formatter (repr o);

let dump_to_channel c o =
  dump_with_formatter (formatter_of_out_channel c) (repr o)

let dump_to_buffer b o =
  dump_with_formatter (formatter_of_buffer b) (repr o)

let dump_to_string o =
  let b = Buffer.create 128 in
    dump_to_buffer b o;
    Buffer.contents b

(*----------------------------------------------------------------------------*)

let wobytes = Sys.word_size / 8
let hdbytes = wobytes

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
      	    for i = 0 to n - 1 do add_candidate (field r i) done;
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
