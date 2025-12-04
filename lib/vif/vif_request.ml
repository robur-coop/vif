let src = Logs.Src.create "vif.request"

module Log = (val Logs.src_log src : Logs.LOG)

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
let tags { request; _ } = Vif_request0.tags request

let to_string { request; _ } =
  let src = Vif_request0.source request in
  Flux.Stream.from src |> Flux.Stream.into Flux.Sink.string

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt
let errorf fmt = Format.kasprintf (fun msg -> `Error msg) fmt

type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]

let jsonm () =
  let decoder = Jsonm.decoder `Manual in
  let rec error (`Error err) =
    errorf "Invalid JSON input: %a" Jsonm.pp_error err
  and end_of_input `End = errorf "Unexpected end of input"
  and arr acc k =
    match Jsonm.decode decoder with
    | #await -> `Await (fun () -> arr acc k)
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> core (fun v -> arr (v :: acc) k) v
  and name n k =
    match Jsonm.decode decoder with
    | #await -> `Await (fun () -> name n k)
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> core (fun v -> k (n, v)) v
  and obj acc k =
    match Jsonm.decode decoder with
    | #await -> `Await (fun () -> obj acc k)
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v ->
        errorf "Unexpected lexeme: %a (expected key)" Jsonm.pp_lexeme v
  and core k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> errorf "Retrieve invalid end of JSON array/object"
    | `Name _ -> errorf "Retrieve invalid JSON key value"
  and init () =
    match Jsonm.decode decoder with
    | #await -> `Await init
    | #error as v -> error v
    | #eoi -> `Json `Null
    | `Lexeme (#Jsonm.lexeme as lexeme) -> core (fun v -> `Json v) lexeme
  in
  let push v str =
    match v with
    | `Await k ->
        Jsonm.Manual.src decoder
          (Bytes.unsafe_of_string str)
          0 (String.length str);
        k ()
    | `Error _ as err -> err
    | `Json _ as value -> value
  in
  let full = function `Error _ | `Json _ -> true | _ -> false in
  let rec stop = function
    | `Await k ->
        Jsonm.Manual.src decoder Bytes.empty 0 0;
        stop (k ())
    | `Error msg -> Error (`Msg msg)
    | `Json value -> Ok value
  in
  Flux.Sink { init; push; full; stop }

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

let of_json : type a.
    ('socket, Vif_type.json, a) t -> (a, [> `Msg of string ]) result = function
  | { encoding= Any; _ } as req -> Ok (to_string req)
  | { encoding= Json; _ } as req ->
      let open Flux in
      let from = source req in
      let res, src = Stream.run ~from ~via:Flow.identity ~into:(jsonm ()) in
      Option.iter Source.dispose src;
      res
  | { encoding= Json_encoding encoding; _ } as req -> begin
      let from = source req in
      let reader = to_reader from in
      match Jsont_bytesrw.decode encoding reader with
      | Error msg ->
          Bytesrw.Bytes.Reader.discard reader;
          Error (`Msg msg)
      | Ok _ as value -> value
    end

let get : type v.
    ('socket, 'cfg, v) Vif_middleware.t -> ('socket, 'a, 'c) t -> v option =
 fun (Vif_middleware.Middleware (_, key)) { env; _ } ->
  Vif_middleware.Hmap.find key env

type 'socket request = 'socket Vif_request0.t

let headers_of_request = Vif_request0.headers
let method_of_request = Vif_request0.meth
let target_of_request = Vif_request0.target
