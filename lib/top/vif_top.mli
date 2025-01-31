type cfg

val config : stdlib:Fpath.t -> string list -> cfg
val eval : cfg -> string list -> (unit, unit) result
