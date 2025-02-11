type t = string

let msgf fmt = Format.kasprintf (fun msg -> `Msg msg) fmt

let for_all f str =
  let rec go acc idx =
    if idx < String.length str then go (f str.[idx] && acc) (succ idx) else acc
  in
  go true 0

let is_upper = function 'A' .. 'Z' -> true | _ -> false
let is_lower = function 'a' .. 'z' -> true | _ -> false

let is_valid_module_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | '-' ->
      true
      (* XXX(dinosaure): an example exists: [First-class-modules].
       [ocamlopt] can compile it but it emits an warning. *)
  | _ -> false

let of_string str =
  if String.length str < 1 then Error (msgf "Invalid empty module name")
  else if
    (is_upper str.[0] || is_lower str.[0]) && for_all is_valid_module_char str
  then Ok str
  else Error (msgf "Invalid module name: %S" str)

let v str =
  match of_string str with Ok v -> v | Error (`Msg err) -> failwith err

let pp ppf t = Format.pp_print_string ppf t
let reflect ppf t = Format.fprintf ppf "(Modname.v %S)" t
let to_string v = v
let compare = String.compare

module Map = Map.Make (String)
