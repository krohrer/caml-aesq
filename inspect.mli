(* Kaspar Rohrer, Thu Apr  8 02:00:21 CEST 2010 *)

type tag =
  | Lazy
  | Closure
  | Object
  | Infix
  | Forward
  | Block
  | Abstract
  | String
  | Double
  | Double_array
  | Custom
  | Int
  | Out_of_heap
  | Unaligned

module Tags : sig
  include Set.S with type elt = tag

  val all : t
  val of_list : tag list -> t
end

val dot : ?tags:Tags.t -> ?follow:Tags.t -> ?max_len:int -> 'a -> unit

val dot_to_file : ?tags:Tags.t -> ?follow:Tags.t -> ?max_len:int -> string -> 'a -> unit

val dump : ?tags:Tags.t -> ?max_depth:int -> 'a -> unit

val dump_to_string : ?tags:Tags.t -> ?max_depth:int -> 'a -> string

val dump_to_buffer : ?tags:Tags.t -> ?max_depth:int -> Buffer.t -> 'a -> unit

val dump_to_channel : ?tags:Tags.t -> ?max_depth:int -> out_channel -> 'a -> unit

val heap_size : ?tags:Tags.t -> ?follow:Tags.t -> 'a -> int
