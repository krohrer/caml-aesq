(* Kaspar Rohrer, Tue Mar 23 06:50:21 CET 2010 *)

exception Empty

type 'a cell =
  | Nil
  | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

let cell x s =
  Cons (x, s)

let cons x s =
  let cell = Cons (x, s) in
    lazy cell

let sing x =
  let cell = Cons (x, lazy Nil) in
    lazy cell

let hd s =
  match Lazy.force s with
    | Nil -> raise Empty
    | Cons (x, _) -> x

let tl s =
  match Lazy.force s with
    | Nil -> raise Empty
    | Cons (_, s) -> s

let rec map f s =
  match Lazy.force s with
    | Nil -> lazy Nil
    | Cons (x, s) -> lazy (Cons (f x, map f s))

let rec fold f a s =
  match Lazy.force s with
    | Nil -> a
    | Cons (x, s) -> fold f (f a x) s

let rec append s sapp =
  match Lazy.force s with
    | Nil -> sapp
    | Cons (x, s) -> lazy (Cons (x, append s sapp))

let flatten slist =
  let rec fold s srest =
    match Lazy.force s with
      | Nil ->
	  begin match srest with
	    | [] -> Nil
	    | s::srest -> fold s srest
	  end
      | Cons (x, s) -> Cons (x, lazy (fold s srest))
  in
    match slist with
      | [] -> lazy Nil
      | s::srest -> lazy (fold s srest)

let rec take n s =
  if n > 0 then
    match Lazy.force s with
      | Nil -> lazy Nil
      | Cons (x, s) -> lazy (Cons (x, take (n - 1) s))
  else
    lazy Nil

let rec drop n s =
  if n > 0 then
    match Lazy.force s with
      | Nil -> lazy Nil
      | Cons (_, s) -> drop (n - 1) s
  else
    s
      
let rec length s =
  let aux sum _ = sum + 1 in
    fold aux 0 s
