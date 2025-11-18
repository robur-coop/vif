let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let port = ref 8080
let inet_addr = ref Unix.inet_addr_loopback
let backlog = ref 64
let pid = ref None
let domains = ref None
let reporter = ref None
let level = ref None

let setup_config domains' port' inet_addr' backlog' pid' reporter' level' =
  port := port';
  inet_addr := inet_addr';
  backlog := backlog';
  pid := pid';
  domains := domains';
  reporter := reporter';
  level := Some level'

let config_from_globals () =
  let sockaddr = Unix.(ADDR_INET (!inet_addr, !port)) in
  Vif_config_unix.config ?reporter:!reporter ?level:!level ?domains:!domains
    ?pid:!pid ~backlog:!backlog sockaddr

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

let is_not_directory str =
  (Sys.file_exists str && Sys.is_directory str = false)
  || Sys.file_exists str = false

let pid =
  let doc = "Specify a file to record its process-id in." in
  let non_existing_file =
    let parser str =
      match Fpath.of_string str with
      | Ok _ as v when is_not_directory str -> v
      | Ok v -> error_msgf "%a already exists as a directory" Fpath.pp v
      | Error _ as err -> err
    in
    Arg.conv (parser, Fpath.pp)
  in
  let open Arg in
  value
  & opt (some non_existing_file) None
  & info [ "pid-file" ] ~doc ~docv:"PATH"

let domains =
  let doc = "The number of number used by vif." in
  let open Arg in
  value & opt (some int) None & info [ "domains" ] ~doc ~docv:"DOMAINS"

let backlog =
  let doc =
    "The limit of outstanding connections in the socket's listen queue."
  in
  let open Arg in
  value & opt int 64 & info [ "backlog" ] ~doc ~docv:"NUMBER"

let docs_output = "OUTPUT"
let t0 = Unix.gettimeofday ()

let reporter sources ppf =
  let re = Option.map Re.compile sources in
  let print src =
    let neg = Fun.negate in
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      let t1 = Unix.gettimeofday () in
      let delta = t1 -. t0 in
      let delta = delta /. 1_000_000_000. in
      Fmt.kpf k ppf
        ("[+%a][%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Blue (fmt "%04.04f"))
        delta
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let res =
        List.fold_left
          (fun acc -> function `Re re -> re :: acc | _ -> acc)
          [] res
      in
      Some (Re.alt res)

let setup_reporter sources utf_8 style_renderer =
  Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Some (reporter sources Fmt.stdout)

let renderer =
  let env = Cmd.Env.info "VIF_FMT" in
  Fmt_cli.style_renderer ~env ~docs:docs_output ()

let regexp : (string * [ `None | `Re of Re.t ]) Arg.conv =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value & opt_all regexp [ ("", `None) ] & info [ "l" ] ~doc ~docv:"REGEXP"

let setup_sources = Term.(const setup_sources $ sources)

let utf_8 =
  let doc = "Allow us to emit UTF-8 characters." in
  let env = Cmd.Env.info "VIF_UTF_8" in
  let open Arg in
  value
  & opt bool true
  & info [ "with-utf-8" ] ~doc ~docv:"BOOL" ~docs:docs_output ~env

let setup_reporter =
  let open Term in
  const setup_reporter $ setup_sources $ utf_8 $ renderer

let verbosity =
  let env = Cmd.Env.info "VIF_LOGS" in
  Logs_cli.level ~env ~docs:docs_output ()

let setup_config =
  let open Term in
  const setup_config
  $ domains
  $ port
  $ inet_addr
  $ backlog
  $ pid
  $ setup_reporter
  $ verbosity
