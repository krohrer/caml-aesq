(* Kaspar Rohrer, Tue Mar 23 06:50:21 CET 2010 *)

exception Empty

type 'a t =
  | Nil
  | Cons of 'a * 'a t Lazy.t

let empty = Nil
let is_empty s = s = Nil

let cons x l =
  Cons (x, l)

let cons2 x y l =
  Cons (x, Lazy.lazy_from_val (Cons (y, l)))

let lcons x f =
  Cons (x, Lazy.lazy_from_fun f)

let hd =
  function
    | Nil -> raise Empty
    | Cons (h,_) -> h

let tl =
  function
    | Nil -> raise Empty
    | Cons (_,t) -> Lazy.force t
