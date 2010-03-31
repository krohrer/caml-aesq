(* Kaspar Rohrer, Tue Mar 23 06:50:21 CET 2010 *)

exception Empty

type 'a cell =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

let cell x s =
  Cons (x, s)

let nil =
  lazy Nil

let cons x s =
  let cell = Cons (x, s) in
    lazy cell

let sing x =
  let cell = Cons (x, nil) in
    lazy cell

let rec from g = lazy (from_aux g)
and from_aux g =
  match g () with
    | None -> Nil
    | Some x -> Cons (x, from g)

let init n f =
  let rec gen i = lazy (gen_aux i)
  and gen_aux i =
    if i < n then Cons (f i, gen (i + 1)) else Nil
  in
    gen 0

let hd (lazy c) =
  match c with
    | Nil -> raise Empty
    | Cons (x, _) -> x

let tl (lazy c) =
  match c with
    | Nil -> raise Empty
    | Cons (_, s) -> s

let rec map f s = lazy (map_aux f s)
and map_aux f (lazy c) =
  match c with
    | Nil -> Nil
    | Cons (x, s) -> Cons (f x, map f s)

let rec fold f a (lazy c) =
  match c with
    | Nil -> a
    | Cons (x, s) -> fold f (f a x) s

let rec iter f (lazy c) =
  match c with
    | Nil -> ()
    | Cons (x, s) -> f x; iter f s

let rec filter p s = lazy (filter_aux p s)
and filter_aux p (lazy c) =
  match c with
  | Nil -> Nil
  | Cons (x, s) ->
      if p x then
	Cons (x, filter p s)
      else
	filter_aux p s

let rec filter_map f s = lazy (filter_map_aux f s)
and filter_map_aux f (lazy c) =
  match c with
    | Nil -> Nil
    | Cons (x, s) ->
	match f x with
	  | None -> filter_map_aux f s
	  | Some x -> Cons (x, filter_map f s)

let rec append s sapp = lazy (append_aux s sapp)
and append_aux (lazy c) sapp =
  match c with
    | Nil -> Lazy.force sapp
    | Cons (x, s) -> Cons (x, append s sapp)

let rec flatten slist =
  match slist with
    | [] -> nil
    | s::rest -> lazy (flatten_aux s rest)
and flatten_aux (lazy c) rest =
  match c with
    | Nil -> Lazy.force (flatten rest)
    | Cons (x, s) -> Cons (x, lazy (flatten_aux s rest))

let rec take n s = lazy (take_aux n s)
and take_aux n (lazy c) =
  match c with
    | Nil -> Nil
    | Cons (x, s) ->
	if n > 0 then
	  Cons (x, take (n - 1) s)
	else
	  Nil

let rec drop n s = lazy (drop_aux n s)
and drop_aux n (lazy c) =
  match c with
    | Nil -> Nil
    | Cons (_, s) ->
	if n > 0 then
	  drop_aux (n - 1) s
	else
	  Lazy.force s

let rec length s =
  let aux sum _ = sum + 1 in
    fold aux 0 s

let is_nil s =
  match Lazy.force s with
    | Nil -> true
    | _ -> false

let is_cons s =
  match Lazy.force s with
    | Cons _ -> true
    | _ -> false
