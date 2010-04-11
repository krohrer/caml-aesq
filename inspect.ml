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
     pointer to a C-struct that identifies the type. *)
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

let tag_id t r = 
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

let value_desc ?(long=false) t r =
  let string_max_length = 8 in
  let string_ellipsis = ".." in
  let abbr = tag_id t r in
    if long then
      match t with
	| Lazy | Forward ->
	    abbr
	| Double ->
	    sprintf "%g" (magic r)
	| Int ->
	    sprintf "%d" (magic r)
	| Block | Closure | Object | Infix | Abstract ->
	    sprintf "%s[%d]" abbr (size r)
	| String ->
	    let s = magic r in
	      if String.length s > string_max_length then
		let n = string_max_length - (String.length string_ellipsis) in
		  sprintf "%S%s" (String.sub s 0 n) string_ellipsis
	      else
		s
	| Double_array -> 
	    sprintf "%s[%d]" abbr (Array.length (magic r))
	| Custom ->
	    sprintf "0x%nX#" (custom_id r)
	| Out_of_heap ->
	    sprintf "0x%nX!" (addr r)
	| Unaligned ->
	    sprintf "0x%nX?" (addr r)
    else
      abbr

(*----------------------------------------------------------------------------*)

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

and dump_with_formatter ?(tags=Tags.all) ?(max_depth=20) fmt o =
  let value2id = HT.create 31337 in
  let wave = Queue.create () in
  let indentation_for_string id = 2 (* String.length id + 2 *) in

  let make_id r =
    let t = tag_of_value r in
    let tid = tag_id t r in
    let n = HT.length value2id in
    let id = sprintf "%s-%X" tid n in
      HT.add value2id r id;
      id

  and find_id r =
    HT.find value2id r
  in

  let sexpr_open fmt id =
    fprintf fmt "@[<hov %d>(%s" (indentation_for_string id) id

  and sexpr_close fmt () =
    fprintf fmt ")@,@]"

  and sexpr_sep fmt () =
    fprintf fmt "@ "

  and sexpr_ref fmt id =
    fprintf fmt "@@%s" id
  in

  let rec sexpr_value ~depth fmt r =
    let t = tag_of_value r in
      match t with
	| Lazy | Closure | Object | Infix | Forward | Block as x when Tags.mem x tags ->
	    sexpr_block ~depth fmt r
	| Double_array as x when Tags.mem x tags ->
	    sexpr_double_array (magic r) fmt r
	| String as x when Tags.mem x tags ->
	    fprintf fmt "%S" (magic r)
	| x ->
	    let long = Tags.mem x tags in
	    let desc = value_desc ~long t r in
	      pp_print_string fmt desc

  and sexpr_block ~depth fmt r =
    try
      sexpr_ref fmt (find_id r)
    with Not_found -> (
      let id = make_id r in
	if depth < max_depth then (
	  let n = size r in
	    sexpr_open fmt id;
	    for i = 0 to n - 1 do
	      sexpr_sep fmt ();
	      sexpr_value ~depth:(depth + 1) fmt (field r i)
	    done;
	    sexpr_close fmt ()
	) else (
	  (* Postpone *)
	  sexpr_ref fmt id;
	  Queue.push r wave
	)
    )

  and sexpr_double_array a fmt r =
    try
      sexpr_ref fmt (find_id r)
    with Not_found -> (
      let id = make_id r in
	sexpr_open fmt id;
	for i = 0 to Array.length a - 1 do
	  let ai = a.(i) in
	  let desc = value_desc ~long:true (tag_of_value ai) (repr ai) in
	    sexpr_sep fmt ();
	    pp_print_string fmt desc;
	done;
	sexpr_close fmt ()
    )

  in
  let values = "DUMP" in
  let r = repr o in
    Queue.push r wave;
    pp_open_vbox fmt 0;
    sexpr_open fmt values;
    while not (Queue.is_empty wave) do
      let r = Queue.pop wave in
	sexpr_sep fmt ();
	sexpr_value ~depth:0 fmt r
    done;
    sexpr_close fmt ();
    pp_close_box fmt ()

(*----------------------------------------------------------------------------*)

type field =
  | Field_link of string * string
  | Field_label of string

let rec dot ?tags ?follow ?max_len o =
  dot_with_formatter ?tags ?follow std_formatter (repr o)

and dot_to_file ?tags ?follow ?max_len path o =
  let oc = open_out path in
    try
      let fmt = formatter_of_out_channel oc in
	dot_with_formatter ?tags ?follow ?max_len fmt (repr o);
	flush oc;
	close_out oc
    with
      | _ -> close_out oc

and dot_with_formatter ?(tags=Tags.all) ?(follow=Tags.all) ?(max_len=0) fmt r =
  let value2id = HT.create 31337 in
  let queue = Queue.create () in

  let rec make_id abbr r =
    try HT.find value2id r with
	Not_found ->
	  let id = sprintf "%s_%d" abbr (HT.length value2id) in
	    HT.add value2id r id;
	    Queue.add (id, r) queue;
	    id

  and make_field t r i =
    assert (tag r < no_scan_tag);
    let f = field r i in
      match tag_of_value f with
	| Lazy | Closure | Object | Infix | Forward | Block as x when Tags.mem t follow ->
	    let long = Tags.mem x tags in
	      Field_link (make_id (tag_id x f) f, value_desc ~long x f)
	| x ->
	    let long = Tags.mem x tags in
	      Field_label (value_desc ~long x f)

  and node_open fmt id =
    fprintf fmt "@[<2>%s@ [" id

  and node_close fmt () =
    fprintf fmt "];@]@,"

  and link_open fmt src dst =
    fprintf fmt "@[<2>%s ->@ %s@ [" src dst

  and link_close fmt () =
    fprintf fmt "];@]@,"

  and attr_open fmt name =
    fprintf fmt "@[<h>%s = \"" name

  and attr_close fmt () =
    fprintf fmt "\",@]@ "

  and attr_label fmt s =
    attr_open fmt "label";
    fprintf fmt "%s" s;
    attr_close fmt ()
  in

  let rec dot_node id fmt r =
    let t = tag_of_value r in
      match t with
	| Lazy | Closure | Object | Infix | Forward | Block as x when Tags.mem x tags ->
	    let n = size r in
	    let too_long = n > max_len in (* Print more info if fields are cut off *)
	    let fields = Array.init n (make_field t r) in
	      node_open fmt id;
	      attr_open fmt "label";
	      fprintf fmt "<hd> %s" (value_desc ~long:too_long x r);
	      Array.iteri dot_label fields;
	      attr_close fmt ();
	      node_close fmt ();
	      Array.iteri (dot_link id) fields
	| x ->
	    node_open fmt id;
	    attr_label fmt (value_desc ~long:true x r);
	    node_close fmt ()

  and dot_label i f =
    if i < max_len then
      match f with
    	| Field_label l ->
    	    fprintf fmt "| %s" l
    	| Field_link (_, l) ->
    	    fprintf fmt "|<f%d> %s" i l
    else if i = max_len && 0 < max_len  then
      fprintf fmt "|<rest> ..."
    else
      ()

  and dot_link id i f =
      match f with
	| Field_label _ ->
	    ()
	| Field_link (lid, _) ->
	    let src =
	      if i < max_len then
	    	sprintf "%s:f%d" id i
	      else if 0 < max_len then
	    	sprintf "%s:rest" id
	      else
		id
	    in
	    let dst = sprintf "%s" lid in
	      link_open fmt src dst;
	      attr_label fmt (string_of_int i);
	      link_close fmt ()

  in
    ignore (make_id "ROOT" r);
    fprintf fmt "@[<v>@[<v 2>digraph {@,";
    fprintf fmt "graph [rankdir=LR, splines=true, overlap=false, sep=0.1];@,";
    fprintf fmt "node [shape=record, style=rounded];@,";
    fprintf fmt "edge [dir=both, arrowtail=odot];@,";
    while not (Queue.is_empty queue) do
      let id, r = Queue.take queue in
	dot_node id fmt r
    done;
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
		( if Tags.mem x tags then
		    add_bytes (hdbytes + n*wobytes);
		  if Tags.mem x follow then
		    for i = 0 to n - 1 do
		      add_candidate (field r i)
		    done
		)
	  | Abstract | String | Double | Double_array | Custom as x->
	      let n = size r in
		( if Tags.mem x tags then
		    add_bytes (hdbytes * n*wobytes)
		)
	  | Int | Out_of_heap | Unaligned ->
	      ()
    done;
    !bytes

(*----------------------------------------------------------------------------*)
