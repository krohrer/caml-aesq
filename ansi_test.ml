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
  let s1 = random_stream 0 (1000*1000) in
  (* let s2 = random_stream 0 (1000) in *)
    (* Text.dump_raw stdout s1; *)
    (* let fb = Text.format ~width:100 ~justification:`block s1 in *)
    (* let fc = Text.format ~width:100 ~justification:`center s2 in *)
    (*   (\* Text.dump stdout fb; *\) *)
    (*   Text.print fmt (LazyStream.flatten [fb; fc; fb]); *)
    Text.print std_formatter (Text.format ~justification:`block s1);
    ()
