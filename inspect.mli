(* Kaspar Rohrer, Thu Apr  8 02:00:21 CEST 2010 *)

type tag =
    [
    | `lazy_tag
    | `closure_tag
    | `object_tag
    | `infix_tag
    | `forward_tag
    | `no_scan_tag
    | `abstract_tag
    | `string_tag
    | `double_tag
    | `double_array_tag
    | `custom_tag
    | `int_tag
    | `out_of_heap_tag
    | `unaligned_tag
    ]

module Tags : Set.S

val all_tags : Tags.t

val dump : ?tags:Tags.t -> ?max_depth:int -> 'a -> unit

val dump_to_string : ?tags:Tags.t -> ?max_depth:int -> 'a -> string

val dump_to_buffer : ?tags:Tags.t -> ?max_depth:int -> Buffer.t -> 'a -> unit

val dump_to_channel : ?tags:Tags.t -> ?max_depth:int -> out_channel -> 'a -> unit

val heap_size : ?tags:Tags.t -> 'a -> int
