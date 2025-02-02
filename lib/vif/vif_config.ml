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
}

let config ?pid ?http ?tls ?(backlog = 64) sockaddr =
  let http =
    match http with
    | Some (`H1 cfg) -> Some (`HTTP_1_1 cfg)
    | Some (`H2 cfg) -> Some (`H2 cfg)
    | Some (`Both (h1, h2)) -> Some (`Both (h1, h2))
    | None -> None
  in
  { http; tls; backlog; sockaddr; pid }
