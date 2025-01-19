type 'a t

val create : int -> 'a t
val put : 'a t -> 'a -> unit
val get : 'a t -> 'a option
val close : 'a t -> unit
