let src = Logs.Src.create "vif"

module Log = (val Logs.src_log src : Logs.LOG)
module Json = Json
module U = Vif_u

module R = struct
  include Vif_r
  open Vif_content_type

  type ('fu, 'return) t =
    | Handler : ('f, 'x) body * ('x, 'r) Vif_u.t -> ('f, 'r) t

  let get t = Handler (Body Null, t)
  let post body t = Handler (Body body, t)
  let route (Handler (body, t)) f = Route (body, t, f)
  let ( --> ) = route
end

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

module Content_type = Vif_content_type
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

let is_application_json { Multipart_form.Content_type.ty; subty; _ } =
  match (ty, subty) with `Application, `Iana_token "json" -> true | _ -> false

let request server =
  let extract : type c a.
      (c, a) Vif_content_type.t -> (c, a) Vif_request.t option = function
    | Vif_content_type.Any as encoding ->
        Some (Vif_request.of_reqd ~encoding server.S.reqd)
    | Null as encoding -> Some (Vif_request.of_reqd ~encoding server.S.reqd)
    | Json_encoding _ as encoding ->
        let headers =
          match server.S.reqd with
          | `V1 reqd ->
              let request = H1.Reqd.request reqd in
              H1.Headers.to_list request.H1.Request.headers
          | `V2 reqd ->
              let request = H2.Reqd.request reqd in
              H2.Headers.to_list request.H2.Request.headers
        in
        let c = List.assoc_opt "content-type" headers in
        let c = Option.map (fun c -> c ^ "\r\n") c in
        let c = Option.to_result ~none:`Not_found c in
        let c = Result.bind c Multipart_form.Content_type.of_string in
        begin
          match c with
          | Ok c when is_application_json c ->
              Some (Vif_request.of_reqd ~encoding server.S.reqd)
          | _ -> None
        end
    | Json as encoding ->
        let headers =
          match server.S.reqd with
          | `V1 reqd ->
              let request = H1.Reqd.request reqd in
              H1.Headers.to_list request.H1.Request.headers
          | `V2 reqd ->
              let request = H2.Reqd.request reqd in
              H2.Headers.to_list request.H2.Request.headers
        in
        let c = List.assoc_opt "content-type" headers in
        let c = Option.map (fun c -> c ^ "\r\n") c in
        let c = Option.to_result ~none:`Not_found c in
        let c = Result.bind c Multipart_form.Content_type.of_string in
        Log.debug (fun m ->
            m "content-type: %a"
              Fmt.(
                Dump.result ~ok:Multipart_form.Content_type.pp
                  ~error:(any "#errored"))
              c);
        begin
          match c with
          | Ok c when is_application_json c ->
              Some (Vif_request.of_reqd ~encoding server.S.reqd)
          | _ -> None
        end
  in
  { Vif_r.extract }

let handler ~default routes devices user's_value socket reqd =
  let target =
    match reqd with
    | `V1 reqd -> (H1.Reqd.request reqd).H1.Request.target
    | `V2 reqd -> (H2.Reqd.request reqd).H2.Request.target
  in
  let server = { Vif_s.reqd; socket; devices } in
  let request = request server in
  Log.debug (fun m -> m "Handle a new request to %s" target);
  let fn = R.dispatch ~default routes ~request ~target in
  match fn server user's_value with Vif_response.Response -> ()

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
