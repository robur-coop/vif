include Stdlib.Digest

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt
let pp ppf t = Format.pp_print_string ppf (Stdlib.Digest.to_hex t)

let of_string str =
  match of_hex str with
  | v -> Ok v
  | exception Invalid_argument _ -> error_msgf "Invalid digest value: %S" str

let length = String.length (string "")

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
