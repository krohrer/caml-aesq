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

let random_attributes =
  random_element [|
    Attributes.make ();
    Attributes.make ~intensity:`bold ();
    Attributes.make ~underline:`single ();
    Attributes.make ~foreground:`blue ~background:`yellow ();
    Attributes.make ~background:`green ~underline:`single ();
    Attributes.make ~foreground:`red ~inverted:true ~intensity:`bold ();
    Attributes.make ~foreground:`magenta ~underline:`single ()
  |]

let random_elem () =
  let r = Random.int 1000 in
    if r < 500 then
      `fragment (random_word ())
    else if r < 990 then
      `break
    else if r < 998 then
      `attributes (random_attributes ())
    else
      `linebreak

let rec random_stream i n =
  lazy begin
    if i < n then
      LazyStream.Cons (random_elem (), random_stream (i + 1) n)
    else
      LazyStream.Nil
  end

let _ = 
  Random.self_init ();
  let s1 = random_stream 0 (2000) in
  let s2 = random_stream 0 (1000) in
  let fill = Attributes.make ~inverted:true ~blink:true () in
  let sep = `seq [|`attributes fill; `space 2|] in
    (* Text.dump_raw stdout s1; *)
    let fb = Text.format ~fill ~width:80 ~just:`block s1 in
    let fc = Text.format ~fill ~width:40 ~just:`center s2 in
    let fr = Text.format ~fill ~width:40 ~just:`right s1 in
    (*   (\* Text.dump stdout fb; *\) *)
    (*   Text.print fmt (LazyStream.flatten [fb; fc; fb]); *)
      ignore [fb; fc];
      let tab =
	Text.tabulate ~fill ~sep [
	  80, fb;
	  40, fc;
	  50, fr
	]
      in
	Text.print std_formatter tab;
	()
