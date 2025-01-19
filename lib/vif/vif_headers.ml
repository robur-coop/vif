type t = (string * string) list

let add_unless_exists hdrs k v =
  if List.mem_assoc k hdrs then hdrs else (k, v) :: hdrs
