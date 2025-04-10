val encode_host : string -> string
val encode_path : string -> string
val encode_query : (string * string list) list -> string
val query_of_target : string -> (string * string list) list
