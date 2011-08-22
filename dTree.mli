(** Decision trees *)

type 'a t

val weight : 'a t -> float

val single : ?weight:float -> 'a -> 'a t
val add : 'a t -> 'a t -> 'a t

val of_array : (float*'a) array -> 'a t

val choose : (float -> float) -> 'a t -> 'a option

