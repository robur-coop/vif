type config = {
    http:
      [ `HTTP_1_1 of H1.Config.t
      | `H2 of H2.Config.t
      | `Both of H1.Config.t * H2.Config.t ]
      option
  ; tls: Tls.Config.server option
  ; backlog: int
  ; sockaddr: Unix.sockaddr
  ; pid: Fpath.t option
  ; cookie_key: Mirage_crypto.AES.GCM.key
  ; domains: int
  ; reporter: Logs.reporter option
  ; level: Logs.level option option
}

let really_bad_secret =
  let open Digestif in
  let hash = SHA256.digest_string "\xde\xad\xbe\xef" in
  let hash = SHA256.to_raw_string hash in
  Mirage_crypto.AES.GCM.of_secret hash

let default_domains = Int.min (Stdlib.Domain.recommended_domain_count ()) 4

let alpn_protocols_error expected =
  Fmt.failwith
    "The given TLS configuration does not accept %s connections, you must \
     specify the [alpn_protocols] field in the TLS configuration with \
     [\"http/1.1\"] and/or with [\"h2\"]."
    expected

let lint_tls_config http tls =
  let { Tls.Config.alpn_protocols; _ } = Tls.Config.of_server tls in
  match http with
  | None | Some (`HTTP_1_1 _) ->
      if not (List.mem "http/1.1" alpn_protocols) then
        alpn_protocols_error "http/1.1"
  | Some (`H2 _) ->
      if not (List.mem "h2" alpn_protocols) then alpn_protocols_error "h2"
  | Some (`Both (_, _)) ->
      if
        (not (List.mem "http/1.1" alpn_protocols))
        || not (List.mem "h2" alpn_protocols)
      then alpn_protocols_error "http/1.1 or h2"

let config ?(domains = default_domains) ?(cookie_key = really_bad_secret) ?pid
    ?reporter ?level ?http ?tls ?(backlog = 64) sockaddr =
  let http =
    match http with
    | Some (`H1 cfg) -> Some (`HTTP_1_1 cfg)
    | Some (`H2 cfg) -> Some (`H2 cfg)
    | Some (`Both (h1, h2)) -> Some (`Both (h1, h2))
    | None -> None
  in
  let () = Option.iter (lint_tls_config http) tls in
  { http; tls; backlog; sockaddr; pid; cookie_key; domains; reporter; level }
