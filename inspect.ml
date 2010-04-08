(* Kaspar Rohrer, Thu Apr  8 02:24:17 CEST 2010 *)

open Format

let rec obj_is_list r =
  if Obj.is_int r then
    r = Obj.repr 0
  else
    if Obj.tag r = 0 && Obj.size r = 2 then
      obj_is_list (Obj.field r 1)
    else
      false

let rec dump fmt r =
  ()

and dump_int fmt r =
  pp_print_int fmt (Obj.magic r)

and dump_opaque fmt str r =
  pp_open_box fmt 2;
  pp_print_string fmt "<";
  pp_print_string fmt str;
  for i = 0 to Obj.size r do
    if i > 0 then
      pp_print_space fmt ();
    dump 
  done;
  pp_print_string fmt ">";
  pp_close_box fmt ()

and dump_list fmt r =
  if Obj.is_int r then
    assert (Obj.magic r = 0)
  else begin
    assert (Obj.tag r = 0 && Obj.size r = 2);
    let head = Obj.field r 0 and tail = Obj.field r 1 in
      dump fmt head;
      if Obj.is_int tail then
	assert (Obj.magic r = 0)
      else begin
	pp_print_string fmt ";";
	pp_print_space fm ();
	dump_list fmt tail
      end
  end
