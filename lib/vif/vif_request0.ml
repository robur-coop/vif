type t = {
    request: request
  ; tls: Tls.Core.epoch_data option
  ; reqd: reqd
  ; socket: socket
  ; on_localhost: bool
}

and reqd = Httpcats.Server.reqd
and socket = [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ]
and request = V1 of H1.Request.t | V2 of H2.Request.t

let of_reqd socket reqd =
  let request =
    match reqd with
    | `V1 reqd -> V1 (H1.Reqd.request reqd)
    | `V2 reqd -> V2 (H2.Reqd.request reqd)
  in
  let tls =
    match socket with `Tls tls -> Tls_miou_unix.epoch tls | `Tcp _ -> None
  in
  let fd =
    match socket with
    | `Tls tls ->
        let fd = Tls_miou_unix.file_descr tls in
        Miou_unix.to_file_descr fd
    | `Tcp fd -> Miou_unix.to_file_descr fd
  in
  let on_localhost =
    match Unix.getpeername fd with
    | Unix.ADDR_UNIX _ -> false
    | Unix.ADDR_INET (inet_addr, _) ->
        inet_addr = Unix.inet_addr_loopback
        || inet_addr = Unix.inet6_addr_loopback
  in
  { request; tls; reqd; socket; on_localhost }

let headers { request; _ } =
  match request with
  | V1 req -> H1.Headers.to_list req.H1.Request.headers
  | V2 req -> H2.Headers.to_list req.H2.Request.headers

let meth { request; _ } =
  match request with
  | V1 req -> req.H1.Request.meth
  | V2 req -> req.H2.Request.meth

let target { request; _ } =
  match request with
  | V1 req -> req.H1.Request.target
  | V2 req -> req.H2.Request.target

let request_body { reqd; _ } =
  match reqd with
  | `V1 reqd -> `V1 (H1.Reqd.request_body reqd)
  | `V2 reqd -> `V2 (H2.Reqd.request_body reqd)

let version { request; _ } = match request with V1 _ -> 1 | V2 _ -> 2
let tls { tls; _ } = tls
let on_localhost { on_localhost; _ } = on_localhost
let reqd { reqd; _ } = reqd
