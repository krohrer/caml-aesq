open Ansi

let random_element arr =
    fun () ->
      let i = Random.int (Array.length arr) in
	arr.(i)

let random_word = 
  random_element [| 
    "BORG";
    "A";
    "ZORG";
    "FLOBNOCK";
    "BERZ";
    "FOO" 
  |]

let random_context =
  random_element [|
    make_context ();
    make_context ~intensity:`bold ();
    make_context ~underline:`single ();
    make_context ~foreground:`blue ~background:`yellow ();
    make_context ~background:`green ~underline:`single ();
    make_context ~foreground:`red ~inverted:true ~intensity:`bold ();
    make_context ~foreground:`magenta ~underline:`single ()
  |]

let random_elem () =
  let r = Random.int 1000 in
    if r < 500 then
      `fragment (random_word ())
    else if r < 990 then
      `break
    else if r < 998 then
      `set_context (random_context ())
    else
      `linebreak

let rec random_stream i n =
  lazy begin
    if i < n then
      SCons (random_elem (), random_stream (i + 1) n)
    else
      SNil
  end

let _ = 
  Random.self_init ();
  let s = random_stream 0 (1000) in
  let a = make stdout in
  let fs = format ~justification:`center s in
    dump_raw stdout s;
    dump stdout fs;
    print a (format ~justification:`left s);
    print a (format ~justification:`center s);
    print a (format ~justification:`right s);
    print a (format ~justification:`block s);
    ()
