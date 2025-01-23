module U = Vif_u
module R = Vif_r
module C = Vif_c
module D = Vif_d
module S = Vif_s

module Ds = struct
  type 'value t =
    | [] : 'value t
    | ( :: ) : ('value, 'a) D.device * 'value t -> 'value t

  let run : Vif_d.t -> 'value t -> 'value -> Vif_d.t =
   fun t lst user's_value ->
    let rec go t = function
      | [] -> t
      | x :: r -> go (Vif_d.run t user's_value x) r
    in
    go t lst

  let finally : Vif_d.t -> unit =
   fun t ->
    let[@warning "-8"] (Vif_d.Devices m) = t in
    let fn (Vif_d.Hmap.B (k, v)) =
      let { Vif_d.Device.finally; _ } = Vif_d.Hmap.Key.info k in
      finally v
    in
    Vif_d.Hmap.iter fn m
end

module Stream = Stream
module Method = Vif_method
module Status = Vif_status
module Headers = Vif_headers
module Request = Vif_request
module Response = Vif_response

type stop = Httpcats.Server.stop

type config = {
    http:
      [ `HTTP_1_1 of H1.Config.t
      | `H2 of H2.Config.t
      | `Both of H1.Config.t * H2.Config.t ]
      option
  ; tls: Tls.Config.server option
  ; backlog: int
  ; stop: stop option
  ; sockaddr: Unix.sockaddr
}

let config ?http ?tls ?(backlog = 64) ?stop sockaddr =
  let http =
    match http with
    | Some (`H1 cfg) -> Some (`HTTP_1_1 cfg)
    | Some (`H2 cfg) -> Some (`H2 cfg)
    | Some (`Both (h1, h2)) -> Some (`Both (h1, h2))
    | None -> None
  in
  { http; tls; backlog; stop; sockaddr }

let stop = Httpcats.Server.stop

let handler ~default routes devices user's_value socket reqd =
  let request = Request.of_reqd reqd in
  let target = Request.target request in
  let server = { Vif_s.reqd; socket; devices } in
  R.dispatch ~default routes ~target server request user's_value

let run ~cfg ~devices ~default routes user's_value =
  let domains = Miou.Domain.available () in
  let handle =
   fun handler ->
    match (cfg.http, cfg.tls) with
    | config, Some tls ->
        Httpcats.Server.with_tls ?stop:cfg.stop ?config ~backlog:cfg.backlog tls
          ~handler cfg.sockaddr
    | Some (`H2 _), None ->
        failwith "Impossible to launch an h2 server without TLS."
    | Some (`Both (config, _) | `HTTP_1_1 config), None ->
        Httpcats.Server.clear ?stop:cfg.stop ~config ~handler cfg.sockaddr
    | None, None -> Httpcats.Server.clear ?stop:cfg.stop ~handler cfg.sockaddr
  in
  let[@warning "-8"] (Vif_d.Devices devices) =
    Ds.run Vif_d.empty devices user's_value
  in
  let handler = handler ~default routes devices user's_value in
  let prm = Miou.async @@ fun () -> handle handler in
  if domains > 0 then
    Miou.parallel handle (List.init domains (Fun.const handler))
    |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm;
  Ds.finally (Vif_d.Devices devices)
