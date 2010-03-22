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
      [];
      [`set_intensity `normal];
      [`set_intensity `bold];
      [`set_underline `none];
      [`set_underline `single];
      [`set_foreground `default];
      [`set_foreground `blue];
      [`set_foreground `red];
      [`set_background `default];
      [`set_background `yellow];
      [`set_background `green];
      [`set_inverted false];
      [`set_inverted true];
      [`set_context ctx1];
      [`set_context ctx2];
    |]

let random_elem () =
  let r = Random.int 60 in
    if r < 30 then
      `fragment (random_word ())
    else if r < 50 then
      `space 1
    else
      `ops (random_ops ())

let random_stream n =
  Stream.from
    (fun i ->
       if i < n then
	 Some (random_elem ())
       else
	 None)

let _ = 
  Random.self_init ();
  let s = random_stream (1000*1000*100) in
  let a = A.make stdout in
    A.Printer.print a s;
    A.reset a ();
    A.flush a ();
    ()
