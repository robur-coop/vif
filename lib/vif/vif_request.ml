let src = Logs.Src.create "vif.request"

module Log = (val Logs.src_log src : Logs.LOG)

type ('c, 'a) t = {
    body: [ `V1 of H1.Body.Reader.t | `V2 of H2.Body.Reader.t ]
  ; encoding: ('c, 'a) Vif_type.t
  ; env: Vif_middleware.Hmap.t
  ; request: Vif_request0.t
}

let of_req0 : type c a.
       encoding:(c, a) Vif_type.t
    -> env:Vif_middleware.Hmap.t
    -> Vif_request0.t
    -> (c, a) t =
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
let close { request; _ } = Vif_request0.close request

let to_string { request; _ } =
  let src = Vif_request0.source request in
  Vif_stream.Stream.from src |> Vif_stream.Stream.into Vif_stream.Sink.string

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

let of_json : type a. (Vif_type.json, a) t -> (a, [> `Msg of string ]) result =
  function
  | { encoding= Any; _ } as req -> Ok (to_string req)
  | { encoding= Json; _ } as req ->
      let open Vif_stream in
      let from = source req in
      let res, src = Stream.run ~from ~via:Flow.identity ~into:(Sink.json ()) in
      Option.iter Source.dispose src;
      res
  | { encoding= Json_encoding encoding; _ } as req -> begin
      let open Vif_stream in
      let from = source req in
      let reader = Source.to_reader from in
      match Jsont_bytesrw.decode encoding reader with
      | Error msg ->
          Bytesrw.Bytes.Reader.discard reader;
          Error (`Msg msg)
      | Ok _ as value -> value
    end

let get : type v. ('cfg, v) Vif_middleware.t -> ('a, 'c) t -> v option =
 fun (Vif_middleware.Middleware (_, key)) { env; _ } ->
  Vif_middleware.Hmap.find key env

type request = Vif_request0.t

let headers_of_request = Vif_request0.headers
let method_of_request = Vif_request0.meth
let target_of_request = Vif_request0.target
