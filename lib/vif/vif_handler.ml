type ('c, 'value) t =
     ('c, string) Vif_request.t
  -> string
  -> Vif_server.t
  -> 'value
  -> (Vif_response.empty, Vif_response.sent, unit) Vif_response.t option

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

let cached_on_client_side req target =
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

module K = struct
  type t = Fpath.t

  let equal = Fpath.equal
  let hash = Hashtbl.hash
end

module V = struct
  type t = { mtime: float; mime: string }

  let weight _ = 1
end

module Cache = Lru.M.Make (K) (V)

let cached_on_server_size stat abs_path cache =
  match Cache.find abs_path cache with
  | Some { mtime; mime } when mtime >= stat.Unix.st_mtime -> Some mime
  | Some _ ->
      Cache.remove abs_path cache;
      None
  | None -> None

let static ?(top = pwd) =
  ();
  let cache = Cache.create ~random:true 0x100 in
  fun req target _server _ ->
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
          if cached_on_client_side req abs_path then
            let* () = Vif_response.with_string req "" in
            Vif_response.respond `Not_modified
          else
            let stat = Unix.stat (Fpath.to_string abs_path) in
            let mime =
              match cached_on_server_size stat abs_path cache with
              | Some mime -> mime
              | None ->
                  let mime = mime_type abs_path in
                  let value = { V.mtime= stat.Unix.st_mtime; mime } in
                  Cache.add abs_path value cache;
                  mime
            in
            let src = Vif_stream.Source.file (Fpath.to_string abs_path) in
            let field = "content-length" in
            let size = string_of_int stat.Unix.st_size in
            let* () = Vif_response.add ~field size in
            let field = "content-type" in
            let* () = Vif_response.add ~field mime in
            let field = "etag" in
            let* () = Vif_response.add ~field (sha256sum abs_path) in
            let* () = Vif_response.with_source req src in
            Vif_response.respond `OK
        in
        Some process
      end
    | _ -> None
