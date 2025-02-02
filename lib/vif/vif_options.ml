let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let port = ref 8080
let inet_addr = ref Unix.inet_addr_loopback
let backlog = ref 64

let setup_config port' inet_addr' backlog' =
  port := port';
  inet_addr := inet_addr';
  backlog := backlog'

let config_from_globals () =
  let sockaddr = Unix.(ADDR_INET (!inet_addr, !port)) in
  Vif_config.config ~backlog:!backlog sockaddr

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

let backlog =
  let doc =
    "The limit of outstanding connections in the socket's listen queue."
  in
  let open Arg in
  value & opt int 64 & info [ "backlog" ] ~doc ~docv:"NUMBER"

let setup_config =
  let open Term in
  const setup_config $ port $ inet_addr $ backlog
