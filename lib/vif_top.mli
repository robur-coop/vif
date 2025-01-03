type cfg

val config : stdlib:Fpath.t -> Fpath.t list -> cfg
val eval : cfg -> string list -> (string list, string list) result
