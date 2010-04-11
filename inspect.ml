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

let string_with_buffer n =
  let b = Buffer.create n in
    ( fun f ->
	Buffer.clear b;
	f b;
	Buffer.contents b
    )

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

and dump_with_formatter ?(tags=Tags.all) ?(max_depth=10) fmt o =
  let queue = Queue.create () in
  let indentation_for_string id = 3 (* String.length id + 2 *) in

  let rec value2id = HT.create 31337
  and id_of_value r =
    try
      id_find r
    with Not_found -> (
      let t = tag_of_value r in
      let tid = tag_id t r in
      let n = HT.length value2id in
      let id = sprintf "%s-%X" tid n in
	HT.add value2id r id;
	id
    )
  and id_find r =
    HT.find value2id r
  in

  let sexpr_open fmt id =
    fprintf fmt "@[<hv %d>(%s" (indentation_for_string id) id

  and sexpr_close fmt () =
    fprintf fmt ")@]"

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
      sexpr_ref fmt (id_find r)
    with Not_found -> (
      let id = id_of_value r in
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
	  Queue.push r queue
	)
    )

  and sexpr_double_array a fmt r =
    try
      sexpr_ref fmt (id_find r)
    with Not_found -> (
      let id = id_of_value r in
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
    pp_open_vbox fmt 0;
    sexpr_open fmt values;
    Queue.push r queue;
    while not (Queue.is_empty queue) do
      let r = Queue.pop queue in
	sexpr_sep fmt ();
	sexpr_value ~depth:0 fmt r
    done;
    sexpr_close fmt ();
    pp_close_box fmt ()

(*----------------------------------------------------------------------------*)

type field =
  | Field_link of string * Obj.t
  | Field_label of string

let rec dot ?tags ?max_len o =
  dot_with_formatter ?tags std_formatter (repr o)

and dot_osx ?tags ?max_len o =
  let basename = Filename.temp_file "camldump" "." in
  let pr = "dot" in
  let format = "pdf" in
  let dotfile = basename ^ "dot" in
  let outfile = basename ^ format in
    dot_to_file ?tags ?max_len dotfile o;
    let dotcmd = sprintf "%s -T%s -o %S %S" pr format outfile dotfile in
    let outcmd = sprintf "open %S" outfile in
    Sys.command dotcmd == 0 &&
      Sys.command outcmd == 0

and dot_to_file ?tags ?max_len path o =
  let oc = open_out path in
    try
      let fmt = formatter_of_out_channel oc in
	dot_with_formatter ?tags ?max_len fmt (repr o);
	flush oc;
	close_out oc
    with
      | _ -> close_out oc

and dot_with_formatter ?(tags=Tags.all) ?(max_len=(-1)) fmt r =
  let queue = Queue.create () in
  let strbuf = string_with_buffer 80 in

  let rec value2id = HT.create 31337
  and id_of_value r =
    try id_find r with Not_found -> (
      let t = tag_of_value r in
      let id = sprintf "%s_%d" (tag_id t r) (HT.length value2id) in
	HT.add value2id r id;
	id
    )
  and id_find r =
    HT.find value2id r
  in

  let rec fields_of_value r =
    if tag r < no_scan_tag then (
      let field_of_value i =
	let f = field r i in
	  match tag_of_value f with
	    | Lazy | Closure | Object | Infix | Forward | Block as x ->
		let long = Tags.mem x tags in
		  Field_link (value_desc ~long x f, f)
	    | x ->
		let long = Tags.mem x tags in
		  Field_label (value_desc ~long x f)
      in
	Array.init (size r) field_of_value
    )
    else (
      [||]
    )

  and fields_to_label id desc fields : string =
    let add_field b i f =
      if i < max_len then
	match fields.(i) with
    	  | Field_label l ->
	      bprintf b "| %s" l
    	  | Field_link (l, _) ->
	      bprintf b "|<f%d> %s" i l
      else if i = max_len && 0 <= max_len then
	bprintf b "|<rest> ..."
      else
	()
    in
    let add_fields b = 
      bprintf b "<hd> %s" desc;
      Array.iteri (add_field b) fields
    in
      strbuf add_fields

  and fields_to_links id fields =
    let links = ref [] in
    let addi_link i =
      function
	| Field_label _ ->
	    ()
	| Field_link (_, f) ->
	    let dst = 
	      try id_find f with Not_found -> (
		let id = id_of_value f in
		  Queue.push f queue;
		  id
	      )
	    in
	    let src = id in
	    let attrs = ("label", string_of_int i) :: [] in
	      links := ((src,dst), attrs) :: !links;
    in
      Array.iteri addi_link fields;
      !links
  in

  let node_open fmt id =
    fprintf fmt "@[<2>%s@ [" id

  and node_close fmt () =
    fprintf fmt "];@]@,"

  and link_open fmt (src, dst) =
    fprintf fmt "@[<2>%s ->@ %s@ [" src dst

  and link_close fmt () =
    fprintf fmt "];@]@,"

  and attr_open fmt name =
    fprintf fmt "@[<h>%s = " name

  and attr_close fmt () =
    fprintf fmt ",@]@ "
  in

  let rec node_one fmt id attrs =
    node_open fmt id;
    attr_list fmt attrs;
    node_close fmt ()

  and link_one fmt (l,attrs) =
    link_open fmt l;
    attr_list fmt attrs;
    link_close fmt ()

  and link_list fmt links =
    List.iter (link_one fmt) links

  and attr_one fmt name value =
    attr_open fmt name;
    fprintf fmt "%S" value;
    attr_close fmt ()

  and attr_list fmt attrs =
      (* The list has to be reversed because of the way Graphviz handles
	 duplicate attributes. *)
      List.iter (fun (k,v) -> attr_one fmt k v) (List.rev attrs)
  in

  let rec value_one fmt r =
    let id = id_of_value r in
    let t = tag_of_value r in
      if Tags.mem t tags then (
	let desc = value_desc ~long:true t r in
	let fields = fields_of_value r in
	let label = fields_to_label id desc fields in
	let links = fields_to_links id fields in
	let attrs = ("label", label) :: [] in
	  node_one fmt id attrs;
	  link_list fmt links
      ) else (
	let label = value_desc ~long:true t r in
	let attrs = ("label", label) :: [] in
	  node_one fmt id attrs
      )
  in

    fprintf fmt "@[<v>@[<v 2>digraph {@,";
    fprintf fmt "graph [rankdir=LR, splines=true, overlap=false, sep=0.1];@,";
    fprintf fmt "node [shape=record, style=rounded];@,";
    fprintf fmt "edge [dir=both, arrowtail=odot];@,";
    Queue.push r queue;
    while not (Queue.is_empty queue) do
      let r = Queue.pop queue in
	value_one fmt r
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
