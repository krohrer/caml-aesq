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
  let s1 = random_stream 0 (100000000) in
  let s2 = random_stream 0 (70000000) in
  let s3 = random_stream 0 (100000000) in
  let fill = Attributes.make ~background:`blue () in
    (* Text.dump_raw stdout s1; *)
    let fb = Text.format ~fill ~width:60 ~just:`block s1 in
    let fc = Text.format ~fill ~width:40 ~just:`center s2 in
    let fr = Text.format ~fill ~width:80 ~just:`right s3 in
    (*   (\* Text.dump stdout fb; *\) *)
    (*   Text.print fmt (LazyStream.flatten [fb; fc; fb]); *)
      ignore [fb; fc; fr];
      let tab =
	LazyStream.flatten [
	  Text.format ~fill ~width:180 ~just:`right (random_stream 0 10);
	  Text.pad ~fill:(Attributes.make ~background:`red ()) ~left:4 ~right:4 ~top:2 ~bottom:2 (
	    Text.tabulate ~fill [
	      Text.pad ~fill ~left:2 ~right:2 fb;
	      Text.pad ~fill ~left:2 ~right:2 fc;
	      Text.pad ~fill ~left:2 ~right:2 fr
	    ]
	  )
	]
      in
	Text.print std_formatter tab;
	()
