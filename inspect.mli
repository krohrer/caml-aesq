(* Kaspar Rohrer, Thu Apr  8 02:00:21 CEST 2010 *)

val dump : 'a -> unit

val dump_to_string : 'a -> string

val dump_to_buffer : Buffer.t -> 'a -> unit

val dump_to_channel : out_channel -> 'a -> unit

val heap_size : 'a -> int
