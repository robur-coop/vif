let src = Logs.Src.create "vif.request"

module Log = (val Logs.src_log src : Logs.LOG)

type ('c, 'a) t = {
    body: [ `V1 of H1.Body.Reader.t | `V2 of H2.Body.Reader.t ]
  ; encoding: ('c, 'a) Vif_t.t
  ; env: Vif_m.Hmap.t
  ; request: Vif_request0.t
}

let of_req0 : type c a.
    encoding:(c, a) Vif_t.t -> env:Vif_m.Hmap.t -> Vif_request0.t -> (c, a) t =
 fun ~encoding ~env request ->
  let body = Vif_request0.request_body request in
  { request; body; encoding; env }

let target { request; _ } = Vif_request0.target request
let meth { request; _ } = Vif_request0.meth request
let version { request; _ } = Vif_request0.version request
let headers { request; _ } = Vif_request0.headers request
let reqd { request; _ } = Vif_request0.reqd request
let source { request; _ } = Vif_request0.source request
let accept { request; _ } = Vif_request0.accept request

let to_string { request; _ } =
  let src = Vif_request0.source request in
  Vif_s.Stream.from src |> Vif_s.Stream.into Vif_s.Sink.string

let destruct : type a. a Json_encoding.encoding -> Json.t -> a =
  Json_encoding.destruct

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let of_json : type a. (Vif_t.json, a) t -> (a, [> `Msg of string ]) result =
  function
  | { encoding= Any; _ } as req -> Ok (to_string req)
  | { encoding= Json; _ } as req ->
      let open Vif_s in
      let from = source req in
      let res, src = Stream.run ~from ~via:Flow.identity ~into:(Sink.json ()) in
      Option.iter Source.dispose src;
      res
  | { encoding= Json_encoding encoding; _ } as req -> begin
      let open Vif_s in
      let from = source req in
      let res, src = Stream.run ~from ~via:Flow.identity ~into:(Sink.json ()) in
      Option.iter Source.dispose src;
      match res with
      | Error (`Msg msg) as err ->
          Log.err (fun m -> m "Invalid JSON: %s" msg);
          err
      | Ok (json : Json.t) -> begin
          try Ok (destruct encoding json)
          with Json_encoding.Cannot_destruct (_, _) ->
            error_msgf "Invalid JSON value (according to its encoding)"
        end
      | exception exn ->
          Log.err (fun m -> m "Invalid JSON: %s" (Printexc.to_string exn));
          error_msgf "Invalid JSON value"
    end

let get : type v. ('cfg, v) Vif_m.t -> ('a, 'c) t -> v option =
 fun (Vif_m.Middleware (_, key)) { env; _ } -> Vif_m.Hmap.find key env

type request = Vif_request0.t

let headers_of_request = Vif_request0.headers
let method_of_request = Vif_request0.meth
let target_of_request = Vif_request0.target
