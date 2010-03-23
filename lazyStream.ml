(* Kaspar Rohrer, Tue Mar 23 06:50:21 CET 2010 *)

exception Empty

type 'a cell =
    Nil
  | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

let empty = Lazy.lazy_from_val Nil
let is_empty lc = Lazy.force lc = Nil

let cons x s =
  Lazy.lazy_from_val (Cons (x, s))

let force s =
  Lazy.force s

let hd s =
  match force s with
    | Nil -> raise Empty
    | Cons (h,_) -> h

let tl s =
  match force s with
    | Nil -> raise Empty
    | Cons (_,t) -> t
