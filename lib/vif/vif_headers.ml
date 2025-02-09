type t = (string * string) list

let add_unless_exists hdrs k v =
  if List.mem_assoc k hdrs then hdrs else (k, v) :: hdrs

let get hdrs key = List.assoc_opt key hdrs
