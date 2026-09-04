let src = Logs.Src.create "vif.request"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type ('socket, 'c, 'a) t = {
    body: [ `V1 of H1.Body.Reader.t | `V2 of H2.Body.Reader.t ]
  ; encoding: ('c, 'a) Vif_type.t
  ; env: Vif_middleware.Hmap.t
  ; request: 'socket Vif_request0.t
}

let of_req0 : type c a.
       encoding:(c, a) Vif_type.t
    -> env:Vif_middleware.Hmap.t
    -> 'socket Vif_request0.t
    -> ('socket, c, a) t =
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
let shutdown { request; _ } = Vif_request0.shutdown request
let tags { request; _ } = Vif_request0.tags request

let to_string { request; _ } =
  let src = Vif_request0.source request in
  Flux.Stream.from src |> Flux.Stream.into Flux.Sink.string

let to_reader (Flux.Source { init; pull; _ }) =
  let src = ref (init ()) in
  let rec fn () =
    match pull !src with
    | Some ("", src') ->
        src := src';
        (fn [@tailcall]) ()
    | Some (str, src') ->
        src := src';
        let first = 0 and length = String.length str in
        Bytesrw.Bytes.Slice.make (Bytes.of_string str) ~first ~length
    | None -> Bytesrw.Bytes.Slice.eod
  in
  Bytesrw.Bytes.Reader.make fn

let is_empty req =
  let hdrs = headers req in
  let content_length =
    Vif_headers.get hdrs "Content-Length"
    |> Option.map String.trim
    |> Option.map String.lowercase_ascii
  in
  let transfer_encoding =
    Vif_headers.get hdrs "Transfer-Encoding"
    |> Option.map String.trim
    |> Option.map String.lowercase_ascii
  in
  match (content_length, transfer_encoding) with
  | Some "0", _ -> true
  | Some _, _ | None, Some _ -> false
  | None, None -> true

let of_json : type a.
    ('socket, Vif_type.json, a) t -> (a, [> `Msg of string ]) result = function
  | { encoding= Type Any; _ } as req -> Ok (to_string req)
  | { encoding= Type (Option Any); _ } as req ->
      if is_empty req then Ok None else Ok (Some (to_string req))
  | { encoding= Type (Json_encoding encoding); _ } as req -> begin
      let from = source req in
      let reader = to_reader from in
      match Jsont_bytesrw.decode encoding reader with
      | exception exn ->
          Bytesrw.Bytes.Reader.discard reader;
          error_msgf "Unexpected exception when decoding JSON: %s"
            (Printexc.to_string exn)
      | Error msg ->
          Bytesrw.Bytes.Reader.discard reader;
          Error (`Msg msg)
      | Ok _ as value -> value
    end
  | { encoding= Type (Option (Json_encoding _)); _ } as req when is_empty req ->
      Ok None
  | { encoding= Type (Option (Json_encoding encoding)); _ } as req -> begin
      let from = source req in
      let reader = to_reader from in
      match Jsont_bytesrw.decode encoding reader with
      | exception exn ->
          Bytesrw.Bytes.Reader.discard reader;
          error_msgf "Unexpected exception when decoding JSON: %s"
            (Printexc.to_string exn)
      | Error msg ->
          Bytesrw.Bytes.Reader.discard reader;
          Error (`Msg msg)
      | Ok value -> Ok (Some value)
    end

let get : type v.
    ('socket, 'cfg, v) Vif_middleware.t -> ('socket, 'a, 'c) t -> v option =
 fun (Vif_middleware.Middleware (_, key)) { env; _ } ->
  Vif_middleware.Hmap.find key env

type 'socket request = 'socket Vif_request0.t

let headers_of_request = Vif_request0.headers
let method_of_request = Vif_request0.meth
let target_of_request = Vif_request0.target
