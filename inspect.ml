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

let addr r =
  let h = Nativeint.of_int (magic r land max_int) in
    Nativeint.add h h
      (* according to ${OCAMLSRC}/byterun/mlvalues.h, integers always
	 have the LSB set, and only the upper (Sys.word_size - 1) bits
	 are used to represent the number. Which means that the actual
	 number is the bits of the value right-shifted by 1. If we
	 want to print the bit-pattern (or address) of the value, we
	 therefore have to make sure that the lower bit is one to make
	 a proper int of it and then multiply by 2. *)

let custom_id r =
  assert (tag r = custom_tag);
  (* According to ${OCAMLSRC}/byterun/custom.h, the first field is a
     pointer to a C-string that identifies the custom value. As KMR
     can not think of a way to convert C-strings to OCaml strings
     (without a detour to the world of C), we simply return the
     address instead. *)
  addr (field r 0)

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
    List.fold_left (fun s t -> add t s) empty tlist

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

let tag_of_value r =
  match tag r with
    | x when x = lazy_tag -> Lazy
    | x when x = closure_tag -> Closure
    | x when x = object_tag -> Object
    | x when x = infix_tag -> Infix
    | x when x = forward_tag -> Forward
    | x when x < no_scan_tag -> Block
    | x when x = abstract_tag -> Abstract
    | x when x = string_tag -> String
    | x when x = double_tag -> Double
    | x when x = double_array_tag -> Double_array
    | x when x = custom_tag -> Custom
    | x when x = int_tag -> Int
    | x when x = out_of_heap_tag -> Out_of_heap
    | x when x = unaligned_tag -> Unaligned
    | x -> failwith (sprintf "OCaml value with unknown tag = %d" x)

let tag_abbr t r = 
  match t with
    | Lazy -> "LAZY"
    | Closure -> "CLOS"
    | Object -> "OBJ"
    | Infix -> "INFX"
    | Forward -> "FWD"
    | Block -> sprintf "BL%d" (tag r)
    | Abstract -> "ABST"
    | String -> "STR"
    | Double -> "DBL"
    | Double_array -> "DBLA"
    | Custom -> "CUST"
    | Int -> "INT"
    | Out_of_heap -> "ADDR"
    | Unaligned -> "ADDR"

let value_abbr r =
  let t = tag_of_value r in
    tag_abbr t r

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
  let wave = Queue.create () in
  let indentation_for_string id = 2 (* String.length id + 2 *) in

  let make_id abbr =
    let n = HT.length dumped_blocks in
      sprintf "%s/%X" abbr n
  in

  let rec dump_int i fmt r = 
    let t = Int in
      if Tags.mem t tags then
	fprintf fmt "%d" i
      else
	fprintf fmt "%s" (tag_abbr t r)

  and dump_out_of_heap a fmt r =
    let t = Out_of_heap in
      if Tags.mem t tags then
	fprintf fmt "0x%nX!" a
      else
	fprintf fmt "%s" (tag_abbr t r)

  and dump_unaligned a fmt r =
    let t = Unaligned in
      if Tags.mem t tags then
	fprintf fmt "0x%nX?" a
      else
	fprintf fmt "%s" (tag_abbr t r)

  and dump_double d fmt r =
    let t = Double in
      if Tags.mem t tags then
	fprintf fmt "%e" d
      else
	fprintf fmt "%s" (tag_abbr t r)

  and dump_block ~depth t fmt r =
    try
      fprintf fmt "@@%s" (HT.find dumped_blocks r)
    with Not_found -> begin
      let id = make_id (tag_abbr t r) in
	if depth < max_depth then begin
	  HT.add dumped_blocks r id;
	  if Tags.mem t tags then
	    begin
	      fprintf fmt "@[<hov %d>(%s" (indentation_for_string id) id;
	      for i = 0 to size r - 1 do
		fprintf fmt "@ ";
		dump_aux ~depth:(depth + 1) fmt (field r i)
	      done;
	      fprintf fmt "@,)@]"
	    end
	  else
	    fprintf fmt "%s:#%d" id (size r)
	end else begin
	  fprintf fmt "@@%s" id;
	  Queue.add r wave
	end
    end

  and dump_double_array a fmt r =
    let t = Double_array in
      try
	fprintf fmt "@@%s" (HT.find dumped_blocks r)
      with Not_found -> begin
	let id = make_id (tag_abbr t r) in
	  HT.add dumped_blocks r id;
	  if Tags.mem t tags then begin
	    fprintf fmt "@[<hov %d>(%s" (indentation_for_string id) id;
	    for i = 0 to Array.length a - 1 do
	      fprintf fmt "@ ";
	      dump_double a.(i) fmt r
	    done;
	    fprintf fmt "@,)@]"
	  end else
	    fprintf fmt "%s:#%d" id (Array.length a)
      end
	
  and dump_abstract fmt r =
    let t = Abstract in
      if Tags.mem t tags then
	fprintf fmt "%s:0x%nX" (tag_abbr t r) (addr r)
      else
	fprintf fmt "%s" (tag_abbr t r)

  and dump_custom fmt r =
    let t = Abstract in
      if Tags.mem t tags then
	fprintf fmt "%s:0x%nX" (tag_abbr t r) (custom_id r)
      else
	fprintf fmt "%s" (tag_abbr t r)

  and dump_string s fmt r =
    let t = String in
      if Tags.mem t tags then
	fprintf fmt "\"%s\"" s
      else
	fprintf fmt "%s:#%d" (tag_abbr t r) (String.length s)

  and dump_aux ~depth fmt r =
    match tag_of_value r with
      | Lazy | Closure | Object | Infix | Forward | Block as x ->
	  dump_block ~depth x fmt r
      | Abstract ->
	  dump_abstract fmt r
      | String ->
	  dump_string (magic r) fmt r
      | Double ->
	  dump_double (magic r) fmt r
      | Double_array ->
	  dump_double_array (magic r) fmt r
      | Custom ->
	  dump_custom fmt r
      | Int ->
	  dump_int (magic r) fmt r
      | Out_of_heap ->
	  dump_out_of_heap (addr r) fmt r
      | Unaligned ->
	  dump_unaligned (addr r) fmt r
  in
    fprintf fmt "@[";
    dump_aux ~depth:0 fmt (repr o);
    fprintf fmt "@]@,";
    while not (Queue.is_empty wave) do
      let r = Queue.take wave in
	if not (HT.mem dumped_blocks r) then begin
	  fprintf fmt "@[";
	  dump_aux ~depth:0 fmt r;
	  fprintf fmt "@]@,";
	end
    done

(*----------------------------------------------------------------------------*)

type dot =
  | Dot_node of string
  | Dot_label of string

let rec dot o =
  dot_with_formatter std_formatter (repr o)

and dot_to_file path o =
  let oc = open_out path in
    try
      let fmt = formatter_of_out_channel oc in
	dot_with_formatter fmt (repr o);
	flush oc;
	close_out oc
    with
      | _ -> close_out oc

and dot_with_formatter ?(tags=Tags.all) fmt r =
  let dotted = HT.create 31337 in
  let dotted_make_id abbr r =
    let id = sprintf "%s_%d" abbr (HT.length dotted) in
      HT.add dotted r id;
      id
  in
  let rec dot fmt r =
    match tag_of_value r with
      | Lazy | Closure | Object | Infix | Forward | Block as x ->
	  dot_block x fmt r
      | Abstract ->
	  dot_abstract fmt r
      | String ->
	  dot_string (magic r) fmt r
      | Double ->
	  dot_double (magic r) fmt r
      | Double_array ->
	  dot_double_array (magic r) fmt r
      | Custom ->
	  dot_custom fmt r
      | Int ->
	  dot_int (magic r) fmt r
      | Out_of_heap ->
	  dot_out_of_heap (addr r) fmt r
      | Unaligned ->
	  dot_unaligned (addr r) fmt r

  and node_open fmt id =
    fprintf fmt "@[<2>%s@ [" id

  and node_close fmt () =
    fprintf fmt "];@]@,"

  and link_open fmt id fid =
    fprintf fmt "@[<2>%s ->@ %s@ [" id fid

  and link_close fmt () = 
    fprintf fmt "];@]@,"

  and attr_open fmt name =
    fprintf fmt "@[<h>%s = \"" name

  (* and attr_sep fmt sep = *)
  (*   fprintf fmt "\", " *)

  and attr_close fmt () =
    fprintf fmt "\"@]"

  and attr_label fmt s =
    attr_open fmt "label";
    fprintf fmt "%s" s;
    attr_close fmt ()

  and dot_block t fmt r =
    let abbr = tag_abbr t r in
    let label =
      if t = Block then
	sprintf "%s%d" abbr (tag r)
      else
	abbr
    in
    let id = dotted_make_id label r in
    let n = size r in
    let fields =
      Array.init n (fun i -> dot fmt (field r i))
    in
      Array.iteri
	(fun i n -> match n with
	   | Dot_node fid ->
	       link_open fmt (sprintf "%s:f%d" id i) fid;
	       link_close fmt ()
	   | Dot_label _ -> ())
	fields;
      node_open fmt id;
      attr_open fmt "label";
      fprintf fmt "%s" abbr;
      Array.iteri
	(fun i n -> match n with
	   | Dot_node _ ->
	       fprintf fmt " |<f%d> %d" i i
	   | Dot_label l -> 
	       fprintf fmt " |<f%d> %s" i l)
	fields;
      attr_close fmt ();
      node_close fmt ();
      Dot_node id

  and dot_generic abbr label fmt r =
    let id = dotted_make_id abbr r in
      node_open fmt id;
      attr_label fmt abbr;
      node_close fmt ();
      Dot_node id

  and dot_abstract fmt r =
    let abbr = tag_abbr Abstract r in
      dot_generic abbr abbr fmt r

  and dot_string s fmt r =
    let abbr = tag_abbr String r in
      dot_generic abbr s fmt r

  and dot_double d fmt r =
    let abbr = tag_abbr Double r in
      dot_generic abbr (sprintf "%f" d) fmt r

  and dot_double_array a fmt r =
    let abbr = tag_abbr Double_array r in
      dot_generic abbr (sprintf "%s #%d" abbr (Array.length a)) fmt r

  and dot_custom fmt r =
    let abbr = tag_abbr Custom r in
      dot_generic abbr (sprintf "%s 0x%nX" abbr (custom_id r)) fmt r

  and dot_int i fmt r =
    Dot_label (sprintf "%d" i)

  and dot_out_of_heap a fmt r =
    Dot_label (sprintf "0x%nX" a)

  and dot_unaligned a fmt r =
    Dot_label (sprintf "0x%nX" a)

  in
    fprintf fmt "@[<v>@[<v 2>digraph {@,";
    fprintf fmt "node [shape=record, style=rounded];@,";
    fprintf fmt "edge [len=3];@,";
    ignore (dot fmt r);
    fprintf fmt "@]@,}@]";
    pp_print_newline fmt ()

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
    Stack.push (repr o) candidates;
    while has_candidates () do
      let r = next_candidate () in
      let t = tag_of_value r in
	match t with
	  | Lazy | Closure | Object | Infix | Forward | Block as x ->
	      let n = size r in
		begin
		  if Tags.mem x tags then
		    add_bytes (hdbytes + n*wobytes);
		  if Tags.mem x follow then
		    for i = 0 to n - 1 do
		      add_candidate (field r i)
		    done
		end
	  | Abstract | String | Double | Double_array | Custom as x->
	      let n = size r in
		begin
		  if Tags.mem x tags then
		    add_bytes (hdbytes * n*wobytes)
		end
	  | Int | Out_of_heap | Unaligned ->
	      ()
    done;
    !bytes

(*----------------------------------------------------------------------------*)
