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
    Ansi.make ();
    Ansi.make ~intensity:`bold ();
    Ansi.make ~underline:`single ();
    Ansi.make ~foreground:`blue ~background:`yellow ();
    Ansi.make ~background:`green ~underline:`single ();
    Ansi.make ~foreground:`red ~inverted:true ~intensity:`bold ();
    Ansi.make ~foreground:`magenta ~underline:`single ()
  |]

let random_elem () =
  let r = Random.int 1000 in
    if r < 500 then
      Text.RFrag (random_word ())
    else if r < 990 then
      Text.RBreak
    else if r < 998 then
      Text.RAttr (random_attributes ())
    else
      Text.RLineBreak

let rec random_stream i n =
  lazy begin
    if i < n then
      LazyList.Cons (random_elem (), random_stream (i + 1) n)
    else
      LazyList.Nil
  end

let _ = 
  Random.self_init ();
  let s1 = random_stream 0 (1000) in
  let s2 = random_stream 0 (700) in
  let s3 = random_stream 0 (1000) in
  let nums01 =
    LazyList.append
      (LazyList.take 7 (LazyList.forever (Text.RFrag "0123456789")))
      (LazyList.sing (Text.RFrag "01234567"))
  in
  let fill = Ansi.make ~background:`blue () in
    (* Text.dump_raw stdout s1; *)
    let fb = Text.format ~fill ~width:25 ~just:`block s1 in
    let fc = Text.format ~fill ~width:20 ~just:`center s2 in
    let fr = Text.format ~fill ~width:30 ~just:`right s3 in
      ignore [fb; fc; fr];
      let tab =
	LazyList.flatten [
	  Text.format ~fill ~width:78 ~just:`right (random_stream 0 10);
	  Text.pad ~fill:(Ansi.make ~background:`red ()) ~left:1 ~right:1 ~top:2 ~bottom:1 (
	    Text.tabulate ~fill [
	      Text.pad ~fill ~left:0 ~right:1 ~top:0 ~bottom:0 fb;
	      Text.pad ~fill ~left:0 ~right:1 fc;
	      Text.pad ~fill ~left:0 ~right:0 ~top:10 fr
	    ]
	  )
	]
      in
      let print ~ansi = 
	let pr = Text.make_printer ~ansi stdout in
	  Text.printf pr "print ~ansi:%b <<<\n" ansi;
	  Text.print_lines pr (Text.format nums01);
	  Text.print_lines pr tab;
	  Text.flush pr;
	  Text.print_lines pr (Text.indent 20 (Text.format ~width:58 ~just:`block s3));
	  Text.flush pr;
	  Text.printf pr ">>>\n";
	  ()
      in
	print ~ansi:true;
	print ~ansi:false;
	()
