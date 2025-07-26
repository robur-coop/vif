let src = Logs.Src.create "vif.response"

module Log = (val Logs.src_log src : Logs.LOG)

type empty = Empty
and filled = Filled
and sent = Sent

type 'a state =
  | Empty : empty state
  | Filled : string Vif_stream.source -> filled state
  | Sent : sent state

let empty = Empty
let filled from = Filled from
let sent = Sent

type ('p, 'q, 'a) t =
  | Add_header : string * string -> ('p, 'p, unit) t
  | Add_unless_exists : string * string -> ('p, 'p, bool) t
  | Set_header : string * string -> ('p, 'p, unit) t
  | Rem_header : string -> ('p, 'p, unit) t
  | Return : 'a -> ('p, 'p, 'a) t
  | Bind : ('p, 'q, 'a) t * ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  | Source : string Vif_stream.source -> (empty, filled, unit) t
  | String : string -> (empty, filled, unit) t
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
  Vif_uri.keval uri fn

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

let with_string ?compression:alg req str =
  let field = "content-length" in
  let* () = add ~field (string_of_int (String.length str)) in
  let* _ = add_unless_exists ~field:"connection" "close" in
  let none = return false in
  let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
  String str

let with_tyxml ?compression:alg req tyxml =
  let none = return false in
  let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
  let field = "transfer-encoding" in
  let v = "chunked" in
  let* _ = add_unless_exists ~field v in
  let field = "content-type" in
  let v = "text/html; charset=utf-8" in
  let* _ = add_unless_exists ~field v in
  let* _ = add_unless_exists ~field:"connection" "close" in
  let source =
    Vif_stream.Source.with_formatter @@ fun ppf ->
    Fmt.pf ppf "%a" (Tyxml.Html.pp ()) tyxml
  in
  Source source

let response ?headers:(hdrs = []) status req0 =
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
        Log.debug (fun m -> m "<- %d byte(s)" (String.length str));
        H1.Body.Writer.write_string body str;
        H1.Body.Writer.flush_with_reason body (fn body);
        body
      in
      let full = H1.Body.Writer.is_closed in
      let stop body =
        Log.debug (fun m -> m "<- close the response body");
        H1.Body.Writer.close body
      in
      (Sink { init; push; full; stop } : (string, unit) Vif_stream.sink)
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
      (Sink { init; push; full; stop } : (string, unit) Vif_stream.sink)

let run : type a p q. Vif_request0.t -> p state -> (p, q, a) t -> q state * a =
 fun req s t ->
  let headers = ref [] in
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
        (Filled (Vif_stream.Source.list [ str ]), ())
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
              (headers, Vif_stream.Flow.deflate ())
          | Some "gzip" ->
              let headers = Vif_headers.rem headers "content-length" in
              let headers =
                Vif_headers.add_unless_exists headers "transfer-encoding"
                  "chunked"
              in
              (headers, Vif_stream.Flow.gzip ())
          | _ -> (headers, Vif_stream.Flow.identity)
        in
        let into = response ~headers status req in
        Log.debug (fun m -> m "run our stream to send a response");
        let (), src = Vif_stream.Stream.run ~from ~via ~into in
        Option.iter Vif_stream.Source.dispose src;
        (Sent, ())
  in
  go s t
