open Format

type 'a cell =
  | Node of 'a t array
  | Leaf of 'a
and 'a t =
    float * 'a cell

let weight (w, _) : float = w

let single ?(weight=1.) v : 'a t =
  weight, Leaf v

let count a : int =
  let rec aux sum =
    function
      | _, Leaf _ -> 1
      | _, Node nodes -> Array.fold_left aux 0 nodes
  in
    aux 0 a

let add (w1, t1) (w2, t2) : 'a t =
  let w12 = w1 +. w2 in
    (w12,
     Node [|w1 , t1;
	    w12, t2|])

let of_array arr : 'a t =
  let n = Array.length arr in
  let wsum = ref 0. in
  let aux i =
    let wi, vi = arr.(i) in
    let w = !wsum +. wi in
      wsum := w;
      w, Leaf vi
  in
  let nodes = Array.init n aux in
    !wsum, Node nodes


let choose f t : 'a option =
  let rec choose_aux x t =
    match t with
      | w, Leaf v when x < w ->
	  Some v
      | w, Node ns when x < w ->
	  binsearch_aux x ns 0 (Array.length ns - 1)
      | _, _ ->
	  None
  and binsearch_aux x ns i0 i1 = 
    match i1 - i0 + 1 with
      | 0 -> None
      | 1 -> choose_aux x ns.(i0)
      | n ->
	  let im = (i0 + i1)/2 in
	  let mw, _ = ns.(im) in
	    if x < mw then
	      binsearch_aux x ns i0 im
	    else
	      binsearch_aux x ns (im+1) i1
  in
    match f (weight t) with
      | x when 0. <= x -> choose_aux x t
      | _ -> None
