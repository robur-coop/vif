include module type of Stdlib.Digest

val pp : Format.formatter -> t -> unit
val of_string : string -> (t, [> `Msg of string ]) result
val length : int

module Map : Map.S with type key = t
