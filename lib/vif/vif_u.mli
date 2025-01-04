type 'a atom = 'a Tyre.t
type ('f, 'r) path

val rel : ('r, 'r) path
val host : string -> ('r, 'r) path
val ( / ) : ('f, 'r) path -> string -> ('f, 'r) path
val ( /% ) : ('f, 'a -> 'r) path -> 'a atom -> ('f, 'r) path

type ('f, 'r) query

val nil : ('r, 'r) query
val any : ('r, 'r) query
val ( ** ) : string * 'a atom -> ('f, 'r) query -> ('a -> 'f, 'r) query

type ('f, 'r) t

val ( /? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
val ( //? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
val ( /?? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t

(**/**)

val keval : ('f, 'r) t -> (string -> 'r) -> 'f
val eval : ('f, string) t -> 'f

type 'a handler = 'a Httpcats.handler
type response = Httpcats.response
type error = Httpcats.error

val request : f:'a handler -> 'a -> ('f, (response * 'a, error) result) t -> 'f
