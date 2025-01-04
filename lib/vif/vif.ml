module U = Vif_u
module R = Vif_r
module C = Vif_c

module Request = struct
  type t = H1 of H1.Request.t | H2 of H2.Request.t

  let of_reqd = function
    | `V1 reqd -> H1 (H1.Reqd.request reqd)
    | `V2 reqd -> H2 (H2.Reqd.request reqd)

  let target = function
    | H1 request -> request.H1.Request.target
    | H2 request -> request.H2.Request.target
end

type config =
  [ `HTTP_1_1 of H1.Config.t
  | `H2 of H2.Config.t
  | `Both of H1.Config.t * H2.Config.t ]

let default _reqd _target = ()

let handler routes _socket reqd =
  let request = Request.of_reqd reqd in
  let target = Request.target request in
  R.dispatch ~default routes ~target reqd

let run ?stop ?config ?backlog ?tls_config routes sockaddr =
  let domains = Miou.Domain.available () in
  let handler = handler routes in
  let fn =
    match (config, tls_config) with
    | _, Some tls_config ->
        fun () ->
          Httpcats.Server.with_tls ?stop ?config ?backlog tls_config ~handler
            sockaddr
    | Some (`H2 _), None ->
        failwith "Impossible to launch an h2 server without TLS."
    | Some (`Both (config, _) | `HTTP_1_1 config), None ->
        fun () -> Httpcats.Server.clear ?stop ~config ~handler sockaddr
    | None, None -> fun () -> Httpcats.Server.clear ?stop ~handler sockaddr
  in
  let prm = Miou.async fn in
  if domains > 0 then
    Miou.parallel fn (List.init domains (Fun.const ()))
    |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm
