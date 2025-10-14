type t = (string * string) list

let mem hdrs key =
  let exception True in
  let key = String.lowercase_ascii key in
  let fn (key', _) =
    if String.lowercase_ascii key' = key then raise_notrace True
  in
  try List.iter fn hdrs; false with True -> true

let add_unless_exists hdrs k v = if mem hdrs k then hdrs else (k, v) :: hdrs

let get hdrs key =
  let exception Found of string in
  let key = String.lowercase_ascii key in
  let fn (key', value) =
    if String.lowercase_ascii key' = key then raise_notrace (Found value)
  in
  try List.iter fn hdrs; None with Found value -> Some value

let rem hdrs key =
  let key = String.lowercase_ascii key in
  let fn acc (key', value) =
    if String.lowercase_ascii key' = key then acc else (key', value) :: acc
  in
  List.fold_left fn [] hdrs |> List.rev

let pp = Fmt.(Dump.list (Dump.pair string string))
