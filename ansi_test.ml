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
    else if r < 990 then
      `break
    else if r < 998 then
      random_ops ()
    else
      `linebreak

let rec random_stream i n =
  if i < n then
    A.SCons (random_elem (), lazy (random_stream (i + 1) n))
  else
    A.SNil

open Printf

let _ = 
  Random.self_init ();
  let s = random_stream 0 (1000) in
  let s' = A.LineSplitter.split ~width:40 s in
  let s'' = A.Justification.justify ~width:40 `block s' in
    ignore s'';
  let a = A.make stdout in
    A.Debug.dump stderr s';
    fprintf stdout "\n================================================================================\n%!";
    A.Debug.dump stderr s'';
    fprintf stdout "\n================================================================================\n%!";
    A.Printer.print a s';
    A.reset a ();
    A.flush a ();
    fprintf stdout "\n================================================================================\n%!";
    A.Printer.print a s'';
    A.reset a ();
    A.flush a ();
    ()
