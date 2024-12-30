include module type of Stdlib.Digest

val pp : t Fmt.t
val of_string : string -> (t, [> `Msg of string ]) result
val length : int

module Map : Map.S with type key = t
