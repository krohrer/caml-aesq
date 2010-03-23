module A = Ansi

let a = A.make stdout

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

let random_ops =
  let ctx1 = A.make_context () in
  let ctx2 = A.make_context () in
    random_element [|
      `set_intensity `normal;
      `set_intensity `bold;
      `set_underline `none;
      `set_underline `single;
      `set_foreground `default;
      `set_foreground `blue;
      `set_foreground `red;
      `set_background `default;
      `set_background `yellow;
      `set_background `green;
      `set_inverted false;
      `set_inverted true;
      `set_context ctx1;
      `set_context ctx2;
    |]

let random_elem () =
  let r = Random.int 1000 in
    if r < 600 then
      `fragment (random_word ())
    else if r < 900 then
      `break
    else if r < 902 then
      `linebreak
    else
      random_ops ()

let rec random_stream i n =
  if i < n then
    A.SCons (random_elem (), lazy (random_stream (i + 1) n))
  else
    A.SNil

let _ = 
  Random.self_init ();
  let s = random_stream 0 (1000) in
  let s' = A.LineSplitter.split ~width:80 s in
  let s'' = A.Justification.justify ~width:80 `left s' in
    ignore s'';
  let a = A.make stdout in
    A.Debug.dump stderr s';
    A.Printer.print a s';
    A.reset a ();
    A.flush a ();
    ()
