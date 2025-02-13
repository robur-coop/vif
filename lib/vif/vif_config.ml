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
}

let really_bad_secret =
  let open Digestif in
  let hash = SHA256.digest_string "\xde\xad\xbe\xef" in
  let hash = SHA256.to_raw_string hash in
  Mirage_crypto.AES.GCM.of_secret hash

let config ?(cookie_key = really_bad_secret) ?pid ?http ?tls ?(backlog = 64)
    sockaddr =
  let http =
    match http with
    | Some (`H1 cfg) -> Some (`HTTP_1_1 cfg)
    | Some (`H2 cfg) -> Some (`H2 cfg)
    | Some (`Both (h1, h2)) -> Some (`Both (h1, h2))
    | None -> None
  in
  { http; tls; backlog; sockaddr; pid; cookie_key }
