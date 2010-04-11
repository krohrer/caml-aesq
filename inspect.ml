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
		sprintf "%S" s
	| Double_array ->
	    sprintf "%s[%d]" abbr (Array.length (magic r))
	| Custom ->
	    sprintf "0x%nX" (custom_id r)
	| Out_of_heap ->
	    sprintf "0x%nX" (addr r)
	| Unaligned ->
	    sprintf "0x%nX" (addr r)
    else
      abbr

(*----------------------------------------------------------------------------*)

type dot_attrs = (string * string) list

class type dot_context =
object
  method graph_attrs : dot_attrs
  method all_nodes_attrs : dot_attrs
  method all_edges_attrs : dot_attrs
  method node_attrs : ?root:bool -> size:int -> string -> tag -> dot_attrs
  method edge_attrs : field:int -> tag -> tag -> dot_attrs

  method should_expand_node : size:int -> tag -> bool
  method should_follow_edge : field:int -> tag -> tag -> bool
  method max_size : int
end

class type dump_context =
object
  method should_expand : tag -> bool
  method max_depth : int
end

let default_dot_context =
object
  method graph_attrs =
    [
      "rankdir", "LR";
      "splines", "true";
      "overlap", "false";
      "sep", "0.1"
    ]
  method all_nodes_attrs =
    [
      "shape", "record";
      "style", "rounded"
    ]
  method all_edges_attrs =
    [
      "dir", "both";
      "arrowtail", "odot"
    ]
  method node_attrs ?(root=false) ~size label t =
    let attrs = 
      ("label", label) :: (
	if root then
	  [
	    "penwidth", "3.0";
	  ]
	else
	  []
      )
    in
      match t with
	| String ->
	    ("color", "green"):: attrs
	| x ->
	    attrs

  method edge_attrs ~field st dt =
    [ "label", string_of_int field ]

  method should_expand_node ~size t = true
  method should_follow_edge ~field t x = true
  method max_size = 5
end

let default_dump_context =
object
  method should_expand t = true
  method max_depth = 20
end

(*----------------------------------------------------------------------------*)

let rec dump ?context o =
  dump_with_formatter ?context std_formatter (repr o)

and dump_to_channel ?context c o =
  dump_with_formatter ?context (formatter_of_out_channel c) (repr o)

and dump_to_buffer ?context b o =
  dump_with_formatter ?context (formatter_of_buffer b) (repr o)

and dump_to_string ?context o =
  let b = Buffer.create 128 in
    dump_to_buffer ?context b o;
    Buffer.contents b

and dump_with_formatter ?(context=default_dump_context) fmt o =
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
      let id = sprintf "%s/%X" tid n in
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
    let long = context#should_expand t in
      match t with
	| Int | Out_of_heap | Unaligned ->
	    pp_print_string fmt (value_desc ~long t r)
	| x when long ->
	    sexpr_block ~depth t fmt r
	| x ->
	    pp_print_string fmt (value_desc ~long t r)

  and sexpr_block ~depth t fmt r =
    try
      sexpr_ref fmt (id_find r)
    with Not_found -> (
      let id = id_of_value r in
	if depth <= context#max_depth then (
	  sexpr_open fmt id;
	  sexpr_block_body ~depth t fmt r;
	  sexpr_close fmt ()
	) else if context#max_depth > 0 then (
	  (* Postpone *)
	  sexpr_ref fmt id;
	  Queue.push r queue
	)
    )

  and sexpr_block_body ~depth t fmt r =
    match t with
      | _ when tag r < no_scan_tag ->
	  let n = size r in
	    for i = 0 to n - 1 do
	      sexpr_sep fmt ();
	      sexpr_value ~depth:(depth + 1) fmt (field r i)
	    done
      | Double ->
	  sexpr_sep fmt ();
	  fprintf fmt "%g" (magic r)
      | Double_array ->
	  let a = magic r in
	    for i = 0 to Array.length a - 1 do
	      sexpr_sep fmt ();
	      fprintf fmt "%g" a.(i)
	    done
      | Custom ->
	  sexpr_sep fmt ();
	  fprintf fmt "0x%nX" (custom_id r)
      | _ ->
	  ()
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

let rec dot ?context o =
  dot_with_formatter ?context std_formatter (repr o)

and dot_osx ?context o =
  let basename = Filename.temp_file "camldump" "." in
  let pr = "dot" in
  let format = "pdf" in
  let dotfile = basename ^ "dot" in
  let outfile = basename ^ format in
    dot_to_file ?context dotfile o;
    let dotcmd = sprintf "%s -T%s -o %S %S" pr format outfile dotfile in
    let outcmd = sprintf "open %S" outfile in
    Sys.command dotcmd == 0 &&
      Sys.command outcmd == 0

and dot_to_file ?context path o =
  let oc = open_out path in
    try
      let fmt = formatter_of_out_channel oc in
	dot_with_formatter ?context fmt (repr o);
	flush oc;
	close_out oc
    with
      | _ -> close_out oc

and dot_with_formatter ?(context=default_dot_context) fmt r =
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

  let node_open fmt id =
    fprintf fmt "@[<2>%s@ [" id

  and node_close fmt () =
    fprintf fmt "];@]@,"

  and link_open fmt id i fid =
    let src = id in
    let dst = fid in
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

  and link_one fmt id i fid attrs =
    link_open fmt id i fid;
    attr_list fmt attrs;
    link_close fmt ()

  and attr_one fmt name value =
    attr_open fmt name;
    fprintf fmt "%S" value;
    attr_close fmt ()

  and attr_list fmt attrs =
      (* The list has to be reversed because of the way Graphviz handles
	 duplicate attributes. *)
      List.iter (fun (k,v) -> attr_one fmt k v) (List.rev attrs)
  in

  let value_to_label_and_links id t r =
    let n = if tag r < no_scan_tag then size r else 0 in
    let links = ref [] in
    let rec iter b i =
      if i < n then (
	let f = field r i in
	let x = tag_of_value f in
	let max_size = context#max_size in
	let long = context#should_expand_node ~size:n x in
	  (
	    match x with
	      | Int | Out_of_heap | Unaligned ->
		  ()
	      | _ -> (
		  if long && context#should_follow_edge ~field:i t x then (
		    let fid =
		      try id_find f with Not_found ->
			Queue.push f queue;
			id_of_value f
		    in
		      links := (id, t, i, fid, x) :: !links;
		  )
		)
	  );
	  if i < max_size then (
	    let desc = value_desc ~long x f in
	      bprintf b "| %s" desc
	  )
	  else if i = max_size && 0 <= max_size then (
	    bprintf b "| ..."
	  )
	  else (
	    ()
	  );
	  iter b (i + 1)
      )
      else
	()
    in
    let bprint b =
      let desc = value_desc ~long:true t r in
	bprintf b "<hd> %s" desc;
	iter b 0
    in
    let label = strbuf bprint in
      (label, !links)
  in

  let rec value_one ?(root=false) fmt id r =
    let t = tag_of_value r in
    let label, links = value_to_label_and_links id t r in
    let size = if tag r < no_scan_tag then size r else 0 in
    let node_attrs = context#node_attrs ~root ~size label t in
    let aux (id, st, i, fid, dt) = 
      let edge_attrs = context#edge_attrs ~field:i st dt in
	link_one fmt id i fid edge_attrs
    in
      node_one fmt id node_attrs;
      List.iter aux links
  in
    
    fprintf fmt "@[<v>@[<v 2>digraph {@,";
    node_one fmt "graph" context#graph_attrs;
    node_one fmt "node" context#all_nodes_attrs;
    node_one fmt "edge" context#all_edges_attrs;
    value_one ~root:true fmt (id_of_value r) r;
    while not (Queue.is_empty queue) do
      let r = Queue.pop queue in
	value_one fmt (id_of_value r) r
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
