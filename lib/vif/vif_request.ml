let src = Logs.Src.create "vif.request"

module Log = (val Logs.src_log src : Logs.LOG)

type ('c, 'a) t = {
    request: [ `V1 of H1.Request.t | `V2 of H2.Request.t ]
  ; body: [ `V1 of H1.Body.Reader.t | `V2 of H2.Body.Reader.t ]
  ; encoding: ('c, 'a) Vif_content_type.t
  ; env: Vif_m.Hmap.t
}

let of_reqd : type c a.
       encoding:(c, a) Vif_content_type.t
    -> env:Vif_m.Hmap.t
    -> Httpcats.Server.reqd
    -> (c, a) t =
 fun ~encoding ~env -> function
  | `V1 reqd ->
      let request = `V1 (H1.Reqd.request reqd) in
      let body = `V1 (H1.Reqd.request_body reqd) in
      { request; body; encoding; env }
  | `V2 reqd ->
      let request = `V2 (H2.Reqd.request reqd) in
      let body = `V2 (H2.Reqd.request_body reqd) in
      { request; body; encoding; env }

let target { request; _ } =
  match request with
  | `V1 request -> request.H1.Request.target
  | `V2 request -> request.H2.Request.target

let meth { request; _ } =
  match request with
  | `V1 request -> request.H1.Request.meth
  | `V2 request -> request.H2.Request.meth

let version { request; _ } = match request with `V1 _ -> 1 | `V2 _ -> 2

let headers { request; _ } =
  match request with
  | `V1 request -> H1.Headers.to_list request.H1.Request.headers
  | `V2 request -> H2.Headers.to_list request.H2.Request.headers

let to_string ~schedule ~close body =
  let buf = Buffer.create 0x7ff in
  let c = Miou.Computation.create () in
  let rec on_eof () =
    close body;
    assert (Miou.Computation.try_return c (Buffer.contents buf))
  and on_read bstr ~off ~len =
    Buffer.add_string buf (Bigstringaf.substring bstr ~off ~len);
    schedule body ~on_eof ~on_read
  in
  schedule body ~on_eof ~on_read;
  Miou.Computation.await_exn c

let to_stream ~schedule ~close body =
  let stream = Stream.Bqueue.create 0x100 in
  let rec on_eof () = close body; Stream.Bqueue.close stream
  and on_read bstr ~off ~len =
    Stream.Bqueue.put stream (Bigstringaf.substring bstr ~off ~len);
    schedule body ~on_eof ~on_read
  in
  schedule body ~on_eof ~on_read;
  stream

let to_string { body; _ } =
  match body with
  | `V1 body ->
      to_string ~schedule:H1.Body.Reader.schedule_read
        ~close:H1.Body.Reader.close body
  | `V2 body ->
      to_string ~schedule:H2.Body.Reader.schedule_read
        ~close:H2.Body.Reader.close body

let to_stream { body; _ } =
  match body with
  | `V1 body ->
      to_stream ~schedule:H1.Body.Reader.schedule_read
        ~close:H1.Body.Reader.close body
      |> Stream.Stream.of_bqueue
  | `V2 body ->
      to_stream ~schedule:H2.Body.Reader.schedule_read
        ~close:H2.Body.Reader.close body
      |> Stream.Stream.of_bqueue

let destruct : type a. a Json_encoding.encoding -> Json.t -> a =
  Json_encoding.destruct

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let of_json : type a.
    (Vif_content_type.json, a) t -> (a, [> `Msg of string ]) result = function
  | { encoding= Any; _ } as req -> Ok (to_string req)
  | { encoding= Json; _ } as req ->
      Log.debug (fun m -> m "Parse the body as a JSON data");
      let stream = to_stream req in
      Stream.Stream.into Stream.Sink.json stream
  | { encoding= Json_encoding encoding; _ } as req -> begin
      let stream = to_stream req in
      match Stream.Stream.into Stream.Sink.json stream with
      | Error (`Msg _) as err -> err
      | Ok (json : Json.t) -> begin
          try Ok (destruct encoding json)
          with Json_encoding.Cannot_destruct (_, _) ->
            error_msgf "Invalid JSON value"
        end
    end

let get : type v. ('cfg, v) Vif_m.t -> ('a, 'c) t -> v option =
 fun (Vif_m.Middleware (_, key)) { env; _ } -> Vif_m.Hmap.find key env

type request = Vif_request0.t

let headers_of_request = Vif_request0.headers
let method_of_request = Vif_request0.meth
let target_of_request = Vif_request0.target
