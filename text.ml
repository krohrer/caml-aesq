(* Kaspar Rohrer, Fri Apr 23 00:11:58 CEST 2010 *)

open ExtLib

type justification = [`left | `center | `right | `block | `none]

let justification_to_string =
  function
    | `none -> "none"
    | `left -> "left"
    | `center -> "center"
    | `right -> "right"
    | `block -> "block"

type raw =
  | RFrag of string
  | RAttr of Ansi.t
  | RBreak
  | RLineBreak

type cooked =
  | CFrag of string
  | CAttr of Ansi.t
  | CSpace of int
  | CSeq of cooked array

type size = int
type line = cooked array * size

let rec measure_line_width elements =
  let aux sum =
    function
      | CFrag f -> sum + String.length f
      | CSpace n -> sum + n
      | CSeq elements -> sum + measure_line_width elements
      | CAttr _ -> sum
  in
    Array.fold_left aux 0 elements

let empty_line =
  ([||], 0)

let make_line elements =
  (elements, measure_line_width elements)

let line_width (_,width) = width

let line_concat lines =
  let elements_array, widths = List.split lines in
  let elements = Array.concat elements_array in
  let width = List.fold_left (+) 0 widths in
    (elements, width)

let min_width = 1 (* Minimum width allowed for formatting *)

(* Chop text up into lines (lines still need to be cooked tough) *)
let rec chop attributes width stream =
  let width = max min_width width in
    chop_aux width attributes stream

(* Closures are a poor mans objects, and [width] is the only
   constant field. *)
and chop_aux width =
  (* Chop text so its lines are no longer than [width] *)
  let rec chop_line
      ~attributes
      (* Current attributes *)
      ?(rem_width=width)
      (* Remaining width *)
      ?(line_rev=[RAttr attributes])
      (* Reverse list of line elements, start each line explicitly
	 with the current attributes. Makes it easier to concat lines
	 later on. *)
      ?dismissables
      (* Line elements including elements after the last printable
	 that might be dismissed, if no printable was to
	 follow before the end of the line. *)
      (lazy cell)
      =
    match cell with
      | LazyList.Nil ->
	  (* Done *)
	  chop_done
	    ~attributes
	    ~line_rev
	    ~dismissables
	    ()
      | LazyList.Cons (x, stream) ->
	  (* Dispatch *)
	  begin match x with
	    | RFrag _ as x ->
		chop_fragment
		  ~attributes
		  ~rem_width
		  ~line_rev
		  ~dismissables
		  x
		  stream
	    | RBreak ->
		chop_break
		  ~attributes
		  ~rem_width
		  ~line_rev
		  ~dismissables
		  stream
	    | RLineBreak ->
		chop_linebreak
		  ~attributes
		  ~line_rev
		  stream
	    | RAttr attributes as x ->
		chop_set_attributes
		  ~attributes
		  ~rem_width
		  ~line_rev
		  ~dismissables
		  x
		  stream
	  end
	    
  (* Make partial line from the remaining elements *)
  and chop_done
      ~attributes
      ~line_rev
      ~dismissables
      ()
      =
    let line =
      make_chopped
	~partial:true
	(Option.default line_rev dismissables)
    in
      LazyList.Cons (line, LazyList.nil)

  (* Chop fragment and justify line if necessary *)
  and chop_fragment
      ~attributes
      ~rem_width
      ~line_rev
      ~dismissables
      fragment
      stream
      =
    let frag = match fragment with RFrag f -> f | _ -> assert false in
    let len = String.length frag in
      if len <= rem_width then
	(* Fragment still fits on this line *)
	chop_line
	  ~attributes
	  ~rem_width:(rem_width - len)
	  ~line_rev:(fragment :: Option.default line_rev dismissables)
	  stream
      else if len > width && rem_width >= min_width then
	(* Fragment must be split anyway, may as well start on this
	   line, if possible. It must always be possible if the line
	   does not yet contain any printable elements. *)
	let line =
	  let frag_left = String.slice ~last:rem_width frag in
	    make_chopped
	      (RFrag frag_left :: Option.default line_rev dismissables)
	and cell =
	  let frag_right = String.slice ~first:rem_width frag in
	    (* Prefix stream for next line with left-overs
	       from current line *)
	    LazyList.Cons (RFrag frag_right,
			     stream)
	in
	  LazyList.Cons (line,
			   lazy (chop_line
				   ~attributes
				   (lazy cell)))
      else
	(* Fragment does not fit on current line, retry on
	   next line. *)
	let line =
	  make_chopped line_rev
	and cell =
	  (* This exact cell already exists, but we dont want to
	     pass it as an argument since it only gets used in this
	     case. So we simply construct it anew. (Thank science we
	     have immutable streams!) *)
	  LazyList.Cons (fragment, stream);
	in
	  LazyList.Cons (line,
			   lazy (chop_line ~attributes (lazy cell)))

  (* Break: Ignore at the beginning of the line, or add it to dismissables. *)
  and chop_break
      ~attributes
      ~rem_width
      ~line_rev
      ~dismissables
      stream
      =
    if width = rem_width then
      (* Simply ignore break at the beginning of the
	 line *)
      chop_line
	~attributes
	~rem_width
	~line_rev
	stream
    else
      (* Line already has more than one column *)
      begin match dismissables with
	| None ->
	    (* Add a dismissable break at cost of (at least) one
	       column, if there is still space left. *)
	    if rem_width > 1 then
	      let dismissables =
		RBreak :: Option.default line_rev dismissables
	      in
		chop_line
		  ~attributes
		  ~rem_width:(rem_width - 1)
		  ~line_rev
		  ~dismissables
		  stream
	    else
	      let line =
		make_chopped line_rev
	      in
		LazyList.Cons (line,
				 lazy (chop_line attributes stream))
	| Some _ ->
	    (* Already has dismissable break, ignore *)
	    chop_line
	      ~attributes
	      ~rem_width
	      ~line_rev
	      ?dismissables
	      stream
      end

  (* Linebreak: justify line without loose breaks and
     continue with next line *)
  and chop_linebreak
      ~attributes
      ~line_rev
      stream
      =
    let line = 
      make_chopped ~partial:true line_rev
    in
      LazyList.Cons (line,
		       lazy (chop_line attributes stream))

  (* Set attributes: Update current attributes and add it to the other elements *)
  and chop_set_attributes
      ~attributes
      ~rem_width
      ~line_rev
      ~dismissables
      attr
      stream
      =
    match dismissables with
      | None ->
	  (* Simply add to current line if we have no
	     loose breaks *)
	  chop_line
	    ~attributes
	    ~rem_width
	    ~line_rev:(attr :: line_rev)
	    stream
      | Some breaks ->
	  (* We have one or more loose breaks after a
	     fragment, so we add it to that list. *)
	  chop_line
	    ~attributes
	    ~rem_width
	    ~line_rev
	    ~dismissables:(attr :: breaks)
	    stream

  (* Make a chopped line *)
  and make_chopped
      ?(partial=false)
      line_rev
      =
    let line = Array.of_list line_rev in
      Array.rev_in_place line;
      line, partial
  in
    fun attributes stream -> lazy (chop_line ~attributes stream)

(*------------------------------------*)
      
(* Convert break to space of width 1 *)
let break_to_space =
  function
    | RFrag f -> CFrag f
    | RAttr a -> CAttr a
    | RBreak -> CSpace 1
    | RLineBreak -> CSpace 0

(* Convert breaks to spaces by distributing [break_space] evenly
   among all spaces. Because widths are integers, we use something
   similar to the Bresenham line-drawing algorithm *)
let break_to_space' ~break_space ~break_count () =
  let a = ref break_space in
    function
      | RFrag f -> CFrag f
      | RAttr a -> CAttr a
      | RBreak ->
	  let x = CSpace (!a / break_count) in
	    a := !a mod break_count + break_space;
	    x
      | RLineBreak -> CSpace 0
	  
(* Measure total width of all printable elements *)
let measure_fragments elements =
  let aux sum =
    function
      | RFrag f -> sum + String.length f
      | _ -> sum
  in
    Array.fold_left aux 0 elements

(* Count number of breaks *)
let count_breaks elements =
  let aux sum =
    function
      | RBreak -> sum + 1
      | _ -> sum
  in
    Array.fold_left aux 0 elements

(* Return an array of justified line elements *)
let justify_line fill width justification (elements, partial) =
  let break_count = count_breaks elements in
  let fragment_width = measure_fragments elements in
  let justification =
    match justification with
      | `block -> if partial || break_count = 0 then `left else `block
      | j -> j
  in
    match justification with
      | `none ->
	  (* Simply convert breaks to spaces, ignore desired width *)
	  (Array.map break_to_space elements, fragment_width + break_count)
      | `left ->
	  (* Add left-over space on the right side *)
	  let space = max 0 (width - fragment_width - break_count) in
          let line =
	    [|
	      CSeq (Array.map break_to_space elements);
	      CAttr fill;
	      CSpace space
	    |]
	  in
	    (line, width)
      | `center ->
	  (* Add a bit of left-over space on both sides *)
	  let space = max 0 (width - fragment_width - break_count) in
	  let left_space = space / 2 in
	  let right_space = space - left_space in
	  let line =
	    [|
	      CAttr fill;
	      CSpace left_space;
	      CSeq (Array.map break_to_space elements);
	      CAttr fill;
	      CSpace right_space
	    |]
	  in
	    (line, width)
      | `right ->
	  (* Add left-over space on the left side *)
	  let space = max 0 (width - fragment_width - break_count) in
	  let line =
	    [|
	      CAttr fill;
	      CSpace space;
	      CSeq (Array.map break_to_space elements)
	    |]
	  in
	    (line, width)
      | `block ->
	  (* Distribute *)
	  assert (break_count > 0);
	  let break_space = width - fragment_width in
	  let aux = break_to_space' ~break_space ~break_count () in
	  let line = Array.map aux elements in
	    (line, width)

let format
    ?(attr=Ansi.default)
    ?(fill=Ansi.default)
    ?(width=78)
    ?(just=`none)
    stream
    =
  let chopped_stream = chop attr width stream in
    LazyList.map (justify_line fill width just) chopped_stream

(*----------------------------------------------------------------------------*)

let max_width_over_all_lines stream =
  let aux m l = max m (line_width l) in
    LazyList.fold aux 0 stream

let width_of_first_line (lazy cell) =
  match cell with
    | LazyList.Nil -> 0
    | LazyList.Cons (x, stream) -> line_width x 

(*----------------------------------------------------------------------------*)

type tab = line LazyList.t * size

let empty_tab width =
  (LazyList.nil, width)

let make_tab width stream =
  (stream, width)

let rec tabulate
    ?(attr=Ansi.default) 
    ?(fill=Ansi.default)
    streams
    =
  let streams = Array.of_list streams in
  let widths = Array.map width_of_first_line streams in
    tabulate_aux fill widths streams
      
and tabulate_aux fill widths streams =
  (* Use mutable state to generate the data for a lazy stream. This
     works because the function is only called once for each cons
     cell and the order of invocations is implicitly given by the
     definition of lazy-streams.
     
     IDEA: Could this be applied to chop as well? Probably, but I do
     not see any immediate benefits. *)
  let count = Array.length streams in
  let exhausted = ref false in
  let rec gen () =
    exhausted := true;
    let elems = Array.make count (CSpace 0) in
      for i = 0 to count - 1 do
	match Lazy.force streams.(i) with
          | LazyList.Nil ->
              elems.(i) <-
		CSeq [|
		  CAttr fill;
		  CSpace widths.(i)
		|]
	  | LazyList.Cons ((elements,width), s) ->
	      widths.(i) <- width;
	      exhausted := false;
	      streams.(i) <- s;
	      elems.(i) <- CSeq elements;
      done;
      if !exhausted then
	LazyList.Nil
      else
	LazyList.Cons (make_line elems,
			 Lazy.lazy_from_fun gen)
  in
    Lazy.lazy_from_fun gen

(*----------------------------------------------------------------------------*)

let rec pad
    ?(fill=Ansi.default)
    ?(left=1)
    ?(right=1)
    ?(top=1)
    ?(bottom=1)
    stream
    =
  let width = width_of_first_line stream in
  let filler =
    let fill_line =
      make_line [|
	CAttr fill;
	CSpace (width + left + right)
      |]
    in
      LazyList.forever fill_line
  in
  let aux (elems,_) =
    make_line [|
      CAttr fill;
      CSpace left;
      CSeq elems;
      CAttr fill;
      CSpace right
    |]
  in
    LazyList.flatten [
      LazyList.take top filler;
      LazyList.map aux stream;
      LazyList.take bottom filler
    ]

let indent
    ?(fill=Ansi.default)
    left
    stream
    =
  pad ~fill ~left ~right:0 ~bottom:0 ~top:0 stream

(*----------------------------------------------------------------------------*)

open Printf

let rec dump_raw outc (lazy cell) =
  match cell with
    | LazyList.Nil ->
	fprintf outc "\n";
	Pervasives.flush outc
    | LazyList.Cons (x, stream) ->
	begin match x with
	  | RFrag f ->
	      fprintf outc "%S " f
	  | RBreak ->
	      fprintf outc "BR "
	  | RLineBreak -> 
	      fprintf outc "LBR\n"
	  | RAttr c ->
	      fprintf outc "ATTRS(%s) " (Ansi.to_string c)
	end;
	dump_raw outc stream

let dump outc =
  let rec dump_stream (lazy cell) =
    match cell with
      | LazyList.Nil ->
	  fprintf outc "\n";
	  Pervasives.flush outc
      | LazyList.Cons ((elements,_), stream) ->
	  Array.iter dump_element elements;
	  fprintf outc "\n";
	  dump_stream stream

  and dump_element =
    function
      | CFrag f ->
	  fprintf outc "%S " f
      | CSpace n ->
	  fprintf outc "SP(%d) " n
      | CAttr c ->
	  fprintf outc "ATTRS(%s) " (Ansi.to_string c)
      | CSeq elements ->
	  fprintf outc "[[ ";
	  Array.iter dump_element elements;
	  fprintf outc "]] "
  in
    dump_stream

type printer = {
  pr_ansi : bool;
  mutable pr_attr : Ansi.t option;
  mutable pr_outc : out_channel
}

let make_printer ?(ansi=true) outc =
  {
    pr_ansi = ansi;
    pr_attr = None;
    pr_outc = outc
  }

let printer_attributes pr =
  pr.pr_attr

let printer_set_attributes pr attr =
  pr.pr_attr <- attr

let print_string pr str =
  output_string pr.pr_outc str

let print_newline pr () =
  output_string pr.pr_outc "\n"
    
let print_ansi pr attr =
  let codes = 
    match pr.pr_attr with 
      | None -> Ansi.to_codes Ansi.default
      | Some pr_attr -> Ansi.codes_of_transition pr_attr attr
  in
  let seq = Ansi.sequence_of_codes codes in
    if pr.pr_ansi then print_string pr seq;
    pr.pr_attr <- Some attr

let printf pr fmt =
  Printf.kprintf (print_string pr) fmt
  
let flush pr =
  if pr.pr_ansi then output_string stdout Ansi.reset_sequence;
  pr.pr_attr <- None;
  flush pr.pr_outc;
  ()

let print_lines pr stream =
  let rec print_stream (lazy cell) =
    match cell with
      | LazyList.Nil ->
	  ()
      | LazyList.Cons ((elements,_), stream) ->
	  Array.iter print_element elements;
	  print_newline pr ();
	  print_stream stream

  and print_element =
    function
      | CFrag f ->
	  print_string pr f
      | CSpace n ->
	  for i = 0 to n-1 do print_string pr " " done
      | CAttr attr ->
	  print_ansi pr attr
      | CSeq elements ->
	  Array.iter print_element elements
  in
    print_stream stream

