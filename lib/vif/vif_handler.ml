type ('c, 'value) t =
     ('c, string) Vif_request.t
  -> string
  -> Vif_g.t
  -> 'value
  -> (Vif_response.e, Vif_response.s, unit) Vif_response.t option

let pwd = Fpath.v (Unix.getcwd ())
let tree = Conan_light.tree

let sha256sum path =
  let path = Fpath.to_string path in
  if Sys.file_exists path = false || Sys.is_directory path then
    invalid_arg "sha256sum";
  let fd = Unix.openfile path Unix.[ O_RDONLY ] 0o644 in
  let finally () = Unix.close fd in
  Fun.protect ~finally @@ fun () ->
  let stat = Unix.fstat fd in
  let ba =
    Unix.map_file fd Bigarray.char Bigarray.c_layout false
      [| stat.Unix.st_size |]
  in
  let ba = Bigarray.array1_of_genarray ba in
  let hash = Digestif.SHA256.digest_bigstring ba in
  Digestif.SHA256.to_hex hash

let mime_type path =
  match Conan_unix.run_with_tree Conan_light.tree (Fpath.to_string path) with
  | Ok m ->
      Option.value ~default:"application/octet-stream" (Conan.Metadata.mime m)
  | Error _ -> "application/octet-stream"
  | exception _ -> "application/octet-stream"

let cache req target =
  let hdrs = Vif_request.headers req in
  let hash = sha256sum target in
  match Vif_headers.get hdrs "if-none-match" with
  | Some hash' -> String.equal hash hash'
  | None -> false

let valid ~top target =
  Fpath.is_prefix top target
  && Sys.file_exists (Fpath.to_string target)
  && Sys.is_directory (Fpath.to_string target) = false

let pp_msg ppf (`Msg msg) = Fmt.string ppf msg

let trim lst =
  let lst = List.drop_while (( = ) "") lst in
  let lst = List.drop_while (( = ) "") (List.rev lst) in
  List.rev lst

let static ?(top = pwd) req target _server _ =
  let target = String.split_on_char '/' target in
  let target = trim target in
  let target = String.concat "/" target in
  let abs_path =
    let ( let* ) = Result.bind in
    let* x = Fpath.of_string target in
    Ok Fpath.(normalize (top // x))
  in
  match (Vif_request.meth req, abs_path) with
  | `GET, Ok abs_path when valid ~top abs_path -> begin
      let ( let* ) = Vif_response.bind in
      let process =
        if cache req abs_path then
          let* () = Vif_response.with_string req "" in
          Vif_response.respond `Not_modified
        else
          let src = Vif_s.Source.file (Fpath.to_string abs_path) in
          let field = "content-type" in
          let* () = Vif_response.add ~field (mime_type abs_path) in
          let stat = Unix.stat (Fpath.to_string abs_path) in
          let field = "content-length" in
          let* () = Vif_response.add ~field (string_of_int stat.Unix.st_size) in
          let field = "etag" in
          let* () = Vif_response.add ~field (sha256sum abs_path) in
          let* () = Vif_response.with_source req src in
          Vif_response.respond `OK
      in
      Some process
    end
  | _ -> None
