let src = Logs.Src.create "vif.response"

module Log = (val Logs.src_log src : Logs.LOG)

type empty = Empty
and filled = Filled
and sent = Sent

type 'a state =
  | Empty : empty state
  | Filled : string Flux.source -> filled state
  | Sent : sent state

let _empty = Empty
let filled from = Filled from
let sent = Sent

type ('p, 'q, 'a) t =
  | Add_header : string * string -> ('p, 'p, unit) t
  | Add_unless_exists : string * string -> ('p, 'p, bool) t
  | Set_header : string * string -> ('p, 'p, unit) t
  | Rem_header : string -> ('p, 'p, unit) t
  | Return : 'a -> ('p, 'p, 'a) t
  | Bind : ('p, 'q, 'a) t * ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  | Source : string Flux.source -> (empty, filled, unit) t
  | String : string -> (empty, filled, unit) t
  | Websocket : (empty, sent, unit) t
  | Respond : Vif_status.t -> (filled, sent, unit) t

let bind x fn = Bind (x, fn)
let respond status = Respond status
let return x = Return x
let add ~field value = Add_header (field, value)
let add_unless_exists ~field value = Add_unless_exists (field, value)
let set ~field value = Set_header (field, value)
let rem ~field = Rem_header field
let ( let* ) = bind
let strf fmt = Format.asprintf fmt

let redirect_to ?(with_get = true) req uri =
  let fn rel =
    let* _ = add_unless_exists ~field:"location" rel in
    match (Vif_request.meth req, with_get) with
    | `GET, true (* GET-to-GET *) -> Respond `Found
    | _, true (* XXX-to-GET *) -> Respond `See_other
    | _, false (* XXX-to-XXX *) -> Respond `Temporary_redirect
  in
  Vif_uri.keval ~slash:true uri fn

module Hdrs = Vif_headers

let can_compress alg req =
  match Vif_request.reqd req with
  | `V1 reqd ->
      let req = H1.Reqd.request reqd in
      let hdrs = req.H1.Request.headers in
      begin
        match H1.Headers.get hdrs "accept-encoding" with
        | None -> false
        | Some str ->
            let algs = String.split_on_char ',' str in
            let algs = List.map String.trim algs in
            List.exists (( = ) alg) algs
      end
  | `V2 reqd ->
      let req = H2.Reqd.request reqd in
      let hdrs = req.H2.Request.headers in
      begin
        match H2.Headers.get hdrs "accept-encoding" with
        | None -> false
        | Some str ->
            let algs = String.split_on_char ',' str in
            let algs = List.map String.trim algs in
            List.exists (( = ) alg) algs
      end

let compression alg req =
  match alg with
  | `DEFLATE when can_compress "deflate" req ->
      let* () = set ~field:"content-encoding" "deflate" in
      let* () = rem ~field:"content-length" in
      return true
  | `Gzip when can_compress "gzip" req ->
      let* () = set ~field:"content-encoding" "gzip" in
      let* () = rem ~field:"content-length" in
      return true
  | _ -> return false

let with_source ?compression:alg req source =
  let none = return false in
  let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
  let field = "transfer-encoding" in
  let v = "chunked" in
  let* _ = add_unless_exists ~field v in
  Source source

let connection_close req =
  match Vif_request.version req with
  | 1 -> add_unless_exists ~field:"connection" "close"
  | _ -> return false

let content_length len =
  add_unless_exists ~field:"content-length" (string_of_int len)

let with_string ?compression:alg req str =
  let* _ = content_length (String.length str) in
  let* _ = connection_close req in
  let none = return false in
  let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
  String str

let with_text ?(utf_8 = true) ?compression req str =
  let field = "content-type" in
  let* () =
    if utf_8 then add ~field "text/plain; charset=utf-8"
    else add ~field "text/plain"
  in
  with_string ?compression req str

let with_tyxml ?compression:alg req tyxml =
  let none = return false in
  let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
  let field = "transfer-encoding" in
  let v = "chunked" in
  let* _ = add_unless_exists ~field v in
  let field = "content-type" in
  let v = "text/html; charset=utf-8" in
  let* _ = add_unless_exists ~field v in
  let* _ = connection_close req in
  let source =
    Flux.Source.with_formatter ~size:0x7ff @@ fun ppf ->
    Fmt.pf ppf "%a" (Tyxml.Html.pp ()) tyxml
  in
  Source source

let with_json ?compression:alg req ?format ?number_format w v =
  let open Flux in
  let fn bqueue =
    let fn slice =
      if Bytesrw.Bytes.Slice.is_eod slice then Bqueue.close bqueue
      else Bqueue.put bqueue (Bytesrw.Bytes.Slice.to_string slice)
    in
    let writer = Bytesrw.Bytes.Writer.make fn in
    let res =
      Jsont_bytesrw.encode ?format ?number_format ~eod:true w v writer
    in
    match res with
    | Ok () -> ()
    | Error msg -> Fmt.failwith "Vif.Response.with_json: %s" msg
  in
  let src = Source.with_task ~size:0x7ff fn in
  let none = return false in
  let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
  let field = "transfer-encoding" in
  let v = "chunked" in
  let* _ = add_unless_exists ~field v in
  let field = "content-type" in
  let v = "application/json; charset=utf-8" in
  let* _ = add_unless_exists ~field v in
  let* _ = connection_close req in
  Source src

let empty =
  let* _ = content_length 0 in
  String ""

let websocket = Websocket

let response ?headers:(hdrs = []) status req0 =
  let src = Vif_request0.src req0 in
  match Vif_request0.reqd req0 with
  | `V1 reqd ->
      let hdrs = H1.Headers.of_list hdrs in
      let status =
        match status with
        | #H1.Status.t as status -> status
        | _ -> invalid_arg "Sink.response: invalid status"
      in
      let resp = H1.Response.create ~headers:hdrs status in
      let init () = H1.Reqd.respond_with_streaming reqd resp in
      let fn body = function
        | `Written -> Miou.yield ()
        | `Closed -> H1.Body.Writer.close body
      in
      let push body str =
        H1.Body.Writer.write_string body str;
        H1.Body.Writer.flush_with_reason body (fn body);
        body
      in
      let full = H1.Body.Writer.is_closed in
      let stop body =
        Logs.debug ~src (fun m -> m "<- close the response body");
        H1.Body.Writer.close body
      in
      (Sink { init; push; full; stop } : (string, unit) Flux.sink)
  | `V2 reqd ->
      let hdrs = H2.Headers.of_list hdrs in
      let resp = H2.Response.create ~headers:hdrs status in
      let init () = H2.Reqd.respond_with_streaming reqd resp in
      let push body str =
        H2.Body.Writer.write_string body str;
        body
      in
      let full _ = false in
      let stop = H2.Body.Writer.close in
      (Sink { init; push; full; stop } : (string, unit) Flux.sink)

let upgrade ?headers:(hdrs = []) req0 =
  match Vif_request0.reqd req0 with
  | `V1 reqd ->
      let hdrs = H1.Headers.of_list hdrs in
      H1.Reqd.respond_with_upgrade reqd hdrs
  | `V2 _ -> assert false

let sha1 =
  let ( $ ) = Fun.compose in
  Digestif.(Base64.encode_string $ SHA1.to_raw_string $ SHA1.digest_string)

let get_nonce req =
  let hdrs = Vif_request0.headers req in
  Vif_headers.get hdrs "sec-websocket-key"

let run : type a p q.
       now:(unit -> int32)
    -> 'socket Vif_request0.t
    -> p state
    -> (p, q, a) t
    -> q state * a =
 fun ~now req s t ->
  let headers = ref [] in
  let src = Vif_request0.src req in
  let rec go : type a p q. p state -> (p, q, a) t -> q state * a =
   fun s t ->
    match (s, t) with
    | state, Bind (x, fn) ->
        let state, x = go state x in
        go state (fn x)
    | state, Return x -> (state, x)
    | state, Add_unless_exists (k, v) -> begin
        match Vif_headers.get !headers k with
        | Some _ -> (state, false)
        | None ->
            headers := (k, v) :: !headers;
            (state, true)
      end
    | state, Add_header (k, v) ->
        headers := (k, v) :: !headers;
        (state, ())
    | state, Rem_header k ->
        headers := Vif_headers.rem !headers k;
        (state, ())
    | state, Set_header (k, v) ->
        headers := (k, v) :: Vif_headers.rem !headers k;
        (state, ())
    | Empty, Source from -> (Filled from, ())
    | Empty, String str ->
        if Vif_request0.version req = 1 then
          headers := Vif_headers.add_unless_exists !headers "connection" "close";
        (Filled (Flux.Source.list [ str ]), ())
    | Empty, Websocket -> begin
        match get_nonce req with
        | None -> assert false (* TODO *)
        | Some nonce ->
            let hdrs1 = H1.Websocket.Handshake.server_headers ~sha1 ~nonce in
            let headers = H1.Headers.to_list hdrs1 in
            upgrade ~headers req; (Sent, ())
      end
    | Filled from, Respond status ->
        let headers = !headers in
        let headers, via =
          match Vif_headers.get headers "content-encoding" with
          | Some "deflate" ->
              let headers = Vif_headers.rem headers "content-length" in
              let headers =
                Vif_headers.add_unless_exists headers "transfer-encoding"
                  "chunked"
              in
              let cfg = Flux_zl.config () in
              let to_bstr = Flux.Flow.bstr ~len:0x7ff in
              let flow = Flux.Flow.compose to_bstr (Flux_zl.deflate cfg) in
              (headers, flow)
          | Some "gzip" ->
              let headers = Vif_headers.rem headers "content-length" in
              let headers =
                Vif_headers.add_unless_exists headers "transfer-encoding"
                  "chunked"
              in
              let mtime = now () in
              let cfg = Flux_gz.config ~mtime () in
              let to_bstr = Flux.Flow.bstr ~len:0x7ff in
              let flow = Flux.Flow.compose to_bstr (Flux_gz.deflate cfg) in
              (headers, flow)
          | _ -> (headers, Flux.Flow.identity)
        in
        Logs.debug ~src (fun m ->
            m "new response with: @[<hov>%a@]" Vif_headers.pp headers);
        let into = response ~headers status req in
        Logs.debug ~src (fun m -> m "run our stream to send a response");
        let (), src = Flux.Stream.run ~from ~via ~into in
        Option.iter Flux.Source.dispose src;
        (Sent, ())
  in
  go s t
