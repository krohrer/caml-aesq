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

let max_depth = ref 100
let ellipsis = ref "..."

let rec dump o =
  dump_aux ~depth:0 std_formatter (repr o);

and dump_with_formatter fmt o =
  dump_aux ~depth:0 fmt (repr o);
  pp_print_newline fmt ()

and dump_to_channel c o =
  dump_with_formatter (formatter_of_out_channel c) (repr o)

and dump_to_buffer b o =
  dump_aux ~depth:0 (formatter_of_buffer b) (repr o)

and dump_to_string o =
  let b = Buffer.create 128 in
    dump_to_buffer b o;
    Buffer.contents b

and dump_ellipsis fmt () =
  fprintf fmt "%s" !ellipsis

and dump_int flag fmt i = 
  match flag with
    | `hex -> fprintf fmt "0x%X" i
    | `dec -> pp_print_int fmt i

and dump_double fmt d =
  fprintf fmt "%e" d

and dump_block name ~depth fmt r =
  assert (tag r < no_scan_tag);
  fprintf fmt "@[<b %d><%s@ " (String.length name + 2) name;
  for i = 0 to size r - 1 do
    if i > 0 then fprintf fmt "@ ";
    let f = field r i in
      dump_aux ~depth fmt f
  done;
  fprintf fmt "@,>@]"

(* and dump_tuple ~depth fmt r = *)
(*   assert (tag r < no_scan_tag); *)
(*   fprintf fmt "@[<b 1>("; *)
(*   for i = 0 to size r - 1 do *)
(*     if i > 0 then *)
(*       fprintf fmt ",@ "; *)
(*     let f = field r i in *)
(*       dump_aux ~depth fmt f *)
(*   done; *)
(*   fprintf fmt "@,)@]" *)

and dump_double_array fmt a =
  fprintf fmt "@[<b 2>[|";
  for i = 0 to Array.length a - 1 do
    if i > 0 then fprintf fmt ";@ ";
    dump_double fmt a.(i)
  done;
  fprintf fmt "@,|]@]"

(* and obj_is_list ~depth r = *)
(*   if depth > !max_depth then true else *)
(*     match tag r with *)
(*       | x when x = int_tag -> *)
(* 	  r = repr 0 *)
(*       | x when x = 0 && size r = 2 -> *)
(* 	  let depth = depth + 1 in *)
(* 	    obj_is_list ~depth (field r 1) *)
(*       | _ -> *)
(* 	  false *)

(* and dump_list ~depth fmt r = *)
(*   let dump' fmt r = *)
(*     dump_aux fmt ~depth (field r 0) *)
(*   in *)
(*   let rec aux ~depth fmt r = *)
(*     if depth > !max_depth then dump_ellipsis fmt () else  *)
(*       let depth = depth + 1 in *)
(* 	match tag r with *)
(* 	  | x when x = int_tag && r = repr 0 -> *)
(* 	      dump_int `hex fmt 0 *)
(* 	  | x when x = 0 && size r = 2 -> *)
(* 	      let depth = depth + 1 in  *)
(* 		dump' fmt (field r 0); *)
(* 		fprintf fmt "::@,"; *)
(* 		aux ~depth fmt (field r 1) *)
(* 	  | _ -> *)
(* 	      failfmt "Can not dump arbitrary OCaml value as list." *)
(*   in *)
(*     fprintf fmt "@[<b 1>["; *)
(*     aux ~depth fmt r; *)
(*     fprintf fmt "@,]@]"; *)

and dump_opaque name fmt r =
  fprintf fmt "<%s:0x%X>" name (addr r)

and dump_custom fmt r =
  (* According to ${OCAMLSRC}/byterun/custom.h, the first
     field is a pointer to a c-string that identifies the
     custom value. As we cannot directly print strings, we
     print the address of the identifier instead. *)
  fprintf fmt "@[<b 1><CUST:@,0x%X>@,@]" (addr (field r 0))

and dump_string fmt s =
  fprintf fmt "\"%s\"" s

and dump_aux ~depth fmt r =
  if depth > !max_depth then dump_ellipsis fmt () else
    let depth = depth + 1 in
      match tag r with
	| x when x = lazy_tag ->
	    dump_block "LAZY" ~depth fmt r
	| x when x = closure_tag ->
	    dump_block "CLOS" ~depth fmt r
	| x when x = object_tag ->
	    dump_block "OBJ" ~depth fmt r
	| x when x = infix_tag ->
	    dump_block "INFIX" ~depth fmt r
	| x when x = forward_tag ->
	    dump_block "FWD" ~depth fmt r
	(* | x when x = 0 -> *)
	(*     if obj_is_list ~depth r then *)
	(*       dump_list ~depth fmt r *)
	(*     else *)
	(*       dump_tuple ~depth fmt r *)
	| x when x < no_scan_tag ->
	    dump_block (sprintf "T%d" x) ~depth fmt r
	| x when x = abstract_tag ->
	    dump_opaque "ABST" fmt r
	| x when x = string_tag ->
	    dump_string fmt (magic r)
	| x when x = double_tag ->
	    dump_double fmt (magic r)
	| x when x = double_array_tag ->
	    dump_double_array fmt (magic r)
	| x when x = custom_tag ->
	    dump_custom fmt r
	(* | x when x = final_tag -> () (* Same as custom_tag *) *)
	| x when x = int_tag ->
	    dump_int `hex fmt (magic r)
	| x when x = out_of_heap_tag ->
	    dump_int `hex fmt (addr r)
	| x when x = unaligned_tag ->
	    dump_int `hex fmt (addr r)
	| x -> failfmt "OCaml value with unknown tag = %d" x

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
