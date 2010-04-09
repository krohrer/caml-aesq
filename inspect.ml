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

open ExtList

let addr r = magic r lsl 1
  (* according to ${OCAMLSRC}/byterun/mlvalues.h, integers always have
     the LSB set, and only the upper (Sys.word_size - 1) bits are used
     to represent the number. Which means that the actual number is
     the bits of the value right-shifted by 1. If we want to print the
     bit-pattern (or address) of the value, we therefore have to
     left-shift by 1. *)

let fail_unknown_value x =
  failwith (Printf.sprintf "OCaml value with unknown tag = %d" x)

(*------------------------------------*)

type tag =
  | Lazy
  | Closure
  | Object
  | Infix
  | Forward
  | Block
  | Abstract
  | String
  | Double
  | Double_array
  | Custom
  | Int
  | Out_of_heap
  | Unaligned

module TagType =
struct
  type t = tag
  let compare = compare
end

module Tags =
struct
  include Set.Make(TagType)

  let of_list tlist =
    List.fold_right add tlist empty

  let all =
    of_list [
      Lazy;
      Closure;
      Object;
      Infix;
      Forward;
      Block;
      Abstract;
      String;
      Double;
      Double_array;
      Custom;
      Int;
      Out_of_heap;
      Unaligned;
    ]
end

let tag_abbr = 
  function
    | Lazy -> "LAZY"
    | Closure -> "CLOS"
    | Object -> "OBJ"
    | Infix -> "INFX"
    | Forward -> "FWD"
    | Block -> "BLK"
    | Abstract -> "ABST"
    | String -> "STR"
    | Double -> "DBL"
    | Double_array -> "DBLA"
    | Custom -> "CUST"
    | Int -> "INT"
    | Out_of_heap -> "ADDR"
    | Unaligned -> "ADDR"

(*------------------------------------*)

let rec dump ?tags ?max_depth o =
  dump_with_formatter ?tags ?max_depth std_formatter (repr o)

and dump_to_channel ?tags ?max_depth c o =
  dump_with_formatter ?tags ?max_depth (formatter_of_out_channel c) (repr o)

and dump_to_buffer ?tags ?max_depth b o =
  dump_with_formatter ?tags ?max_depth (formatter_of_buffer b) (repr o)

and dump_to_string ?tags ?max_depth o =
  let b = Buffer.create 128 in
    dump_to_buffer ?tags ?max_depth b o;
    Buffer.contents b

and dump_with_formatter ?(tags=Tags.all) ?(max_depth=30) fmt o =
  let dumped_blocks = HT.create 31337 in
  let postponed_blocks = Queue.create () in
  let indentation_for_string id = 2 (* String.length id + 2 *) in

  let rec dump_int fmt i = 
    let t = Int in
      if Tags.mem t tags then
	fprintf fmt "%d" i
      else
	fprintf fmt "%s" (tag_abbr t)

  and dump_out_of_heap fmt a =
    let t = Out_of_heap in
      if Tags.mem t tags then
	fprintf fmt "0x%X!" a
      else
	fprintf fmt "%s" (tag_abbr t)

  and dump_unaligned fmt a =
    let t = Unaligned in
      if Tags.mem t tags then
	fprintf fmt "0x%X?" a
      else
	fprintf fmt "%s" (tag_abbr t)

  and dump_double fmt d =
    let t = Double in
      if Tags.mem t tags then
	fprintf fmt "%e" d
      else
	fprintf fmt "%s" (tag_abbr t)

  and dump_block ~depth t fmt r =
    try
      fprintf fmt "@@%s" (HT.find dumped_blocks r)
    with Not_found -> begin
      let is_cons = tag r = 0 && size r = 2 in
      let id =
	let n = HT.length dumped_blocks in
	  if t = Block then
	    if is_cons then
	      sprintf ">%X<" n
	    else
	      sprintf "B%d/%X" (tag r) n
	  else
	    sprintf "%s/%X" (tag_abbr t) n
      in
	if depth < max_depth then begin
	  HT.add dumped_blocks r id;
	  if Tags.mem t tags then
	    if is_cons then begin
	      dump_aux ~depth fmt (field r 0);
	      fprintf fmt "@ %s@ " id;
	      dump_aux ~depth fmt (field r 1)
	    end else begin
	      fprintf fmt "@[<hov %d>(%s" (indentation_for_string id) id;
	      for i = 0 to size r - 1 do
		fprintf fmt "@ ";
		dump_aux ~depth:(depth + 1) fmt (field r i)
	      done;
	      fprintf fmt "@,)@]"
	    end
	  else
	    if is_cons then
	      fprintf fmt "%s" id
	    else
	      fprintf fmt "%s:#%d" id (size r)
	end else begin
	  fprintf fmt "@@%s" id;
	  Queue.add r postponed_blocks
	end
    end

  and dump_double_array a fmt r =
    let t = Double_array in
      try
	fprintf fmt "@@%s" (HT.find dumped_blocks r)
      with Not_found -> begin
	let id = sprintf "%s/%X" (tag_abbr t) (HT.length dumped_blocks) in
	  HT.add dumped_blocks r id;
	  if Tags.mem t tags then begin
	    fprintf fmt "@[<hov %d>(%s" (indentation_for_string id) id;
	    for i = 0 to Array.length a - 1 do
	      fprintf fmt "@ ";
	      dump_double fmt a.(i)
	    done;
	    fprintf fmt "@,)@]"
	  end else
	    fprintf fmt "%s:#%d" id (Array.length a)
      end
	
  and dump_abstract fmt r =
    let t = Abstract in
      if Tags.mem t tags then
	fprintf fmt "%s:0x%X" (tag_abbr t) (addr r)
      else
	fprintf fmt "%s" (tag_abbr t)

  and dump_custom fmt r =
    (* According to ${OCAMLSRC}/byterun/custom.h, the first
       field is a pointer to a c-string that identifies the
       custom value. As we cannot directly print strings, we
       print the address of the identifier instead. *)
    let t = Abstract in
      if Tags.mem t tags then
	fprintf fmt "%s:0x%X" (tag_abbr t) (addr (field r 0))
      else
	fprintf fmt "%s" (tag_abbr t)

  and dump_string fmt s =
    let t = String in
      if Tags.mem t tags then
	fprintf fmt "\"%s\"" s
      else
	fprintf fmt "%s:#%d" (tag_abbr t) (String.length s)

  and dump_aux ~depth fmt r =
    match tag r with
      | x when x = lazy_tag ->
	  dump_block ~depth Lazy fmt r
      | x when x = closure_tag ->
	  dump_block ~depth Closure fmt r
      | x when x = object_tag ->
	  dump_block ~depth Object fmt r
      | x when x = infix_tag ->
	  dump_block ~depth Infix fmt r
      | x when x = forward_tag ->
	  dump_block ~depth Forward fmt r
      | x when x < no_scan_tag ->
	  dump_block ~depth Block fmt r
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
	  fail_unknown_value x
  in
    fprintf fmt "@[";
    dump_aux ~depth:0 fmt (repr o);
    fprintf fmt "@]@,";
    while not (Queue.is_empty postponed_blocks) do
      let r = Queue.take postponed_blocks in
	if not (HT.mem dumped_blocks r) then begin
	  fprintf fmt "@[";
	  dump_aux ~depth:0 fmt r;
	  fprintf fmt "@]@,";
	end
    done

(*----------------------------------------------------------------------------*)

let wobytes = Sys.word_size / 8
let hdbytes = wobytes

let heap_size ?(tags=Tags.all) ?(follow=Tags.all) o =
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
  let bytes = ref 0 in
  let add_bytes d =
    bytes := !bytes + d
  in
  let add_block t r =
    let n = size r in
      begin
	if Tags.mem t tags then
	  add_bytes (hdbytes + n*wobytes);
	if Tags.mem t follow then
	  for i = 0 to n - 1 do
	    add_candidate (field r i)
	  done
      end
  in
  let add_no_scan t r =
    let n = size r in
      begin
	if Tags.mem t tags then
	  add_bytes (hdbytes + n*wobytes)
      end
  in
    Stack.push (repr o) candidates;
    while has_candidates () do
      let r = next_candidate () in
	match tag r with

	  | x when x = lazy_tag ->
	      add_block Lazy r
	  | x when x = closure_tag ->
	      add_block Closure r
	  | x when x = object_tag ->
	      add_no_scan Object r
	  | x when x = infix_tag ->
	      add_no_scan Infix r
	  | x when x = forward_tag ->
	      add_block Forward r
	  | x when x < no_scan_tag ->
	      add_block Block r

	  | x when x = abstract_tag ->
	      add_no_scan Abstract r
	  | x when x = string_tag ->
	      add_no_scan String r
	  | x when x = double_tag ->
	      add_no_scan Double r
	  | x when x = double_array_tag ->
	      add_no_scan Double r
	  | x when x = custom_tag ->
	      add_no_scan Custom r

	  | x when x = int_tag ->
	      add_bytes 0
	  | x when x = out_of_heap_tag ->
	      add_bytes 0
	  | x when x = unaligned_tag ->
	      add_bytes 0
	  | x ->
	      fail_unknown_value x
    done;
    !bytes

