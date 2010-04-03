(* Kaspar Rohrer, Sat Apr  3 13:54:45 CEST 2010 *)

open ExtLib

type 'a t = {
  mutable t_data : 'a option;
  mutable t_children : (char * 'a t) list;
}

let make () =
  {
    t_data = None;
    t_children = [];
  }

let rec copy trie =
  {
    t_data = trie.t_data;
    t_children = List.map (fun (c,t) -> c, copy t) trie.t_children;
  }

let replace trie str data =
  let n = String.length str in
  let t = ref trie in
  let i = ref 0 in
    while !i < n do
      let c = str.[!i] and trie = !t in
	begin try
	  t := List.assoc c trie.t_children
	with
	  | Not_found ->
	      t := make ();
	      trie.t_children <- (c, !t) :: trie.t_children
	end;
	i := !i + 1
    done;
    !t.t_data <- Some data

let find trie str =
  let n = String.length str in
  let t = ref trie in
  let i = ref 0 in
    while !i < n do
      let c = str.[!i] and trie = !t in
	t := List.assoc c trie.t_children;
	i := !i + 1
    done;
    match !t.t_data with
      | Some d -> d
      | None -> raise Not_found
