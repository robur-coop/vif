let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let port = ref 8080
let inet_addr = ref Unix.inet_addr_loopback
let backlog = ref 64
let pid = ref None

let setup_config port' inet_addr' backlog' pid' =
  port := port';
  inet_addr := inet_addr';
  backlog := backlog';
  pid := pid'

let config_from_globals () =
  let sockaddr = Unix.(ADDR_INET (!inet_addr, !port)) in
  Vif_config.config ?pid:!pid ~backlog:!backlog sockaddr

open Cmdliner

let port =
  let doc = "The port used by the HTTP server." in
  let open Arg in
  value & opt int 8080 & info [ "p"; "port" ] ~doc ~docv:"PORT"

let inet_addr =
  let doc = "The address to bind the HTTP server." in
  let parser str =
    try Ok (Unix.inet_addr_of_string str)
    with _ -> error_msgf "Invalid inet-addr: %S" str
  in
  let pp ppf inet_addr = Fmt.string ppf (Unix.string_of_inet_addr inet_addr) in
  let inet_addr = Arg.conv (parser, pp) in
  let open Arg in
  value
  & opt inet_addr Unix.inet_addr_loopback
  & info [ "i"; "inet-addr" ] ~doc ~docv:"INET_ADDR"

let pid =
  let doc = "Specify a file to record its process-id in." in
  let non_existing_file =
    let parser str =
      match Fpath.of_string str with
      | Ok _ as v when Sys.file_exists str = false -> v
      | Ok v -> error_msgf "%a already exists" Fpath.pp v
      | Error _ as err -> err
    in
    Arg.conv (parser, Fpath.pp)
  in
  let open Arg in
  value
  & opt (some non_existing_file) None
  & info [ "pid-file" ] ~doc ~docv:"PATH"

let backlog =
  let doc =
    "The limit of outstanding connections in the socket's listen queue."
  in
  let open Arg in
  value & opt int 64 & info [ "backlog" ] ~doc ~docv:"NUMBER"

let setup_config =
  let open Term in
  const setup_config $ port $ inet_addr $ backlog $ pid
