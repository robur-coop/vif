let src = Logs.Src.create "vif.response"

module Log = (val Logs.src_log src : Logs.LOG)

type e = Empty
and f = Filled
and s = Sent

type 'a state =
  | Empty : e state
  | Filled : string Stream.stream -> f state
  | Sent : s state

let empty = Empty
let filled stream = Filled stream
let sent = Sent

type ('p, 'q, 'a) t =
  | Add_header : string * string -> ('p, 'p, unit) t
  | Add_unless_exists : string * string -> ('p, 'p, bool) t
  | Set_header : string * string -> ('p, 'p, unit) t
  | Rem_header : string -> ('p, 'p, unit) t
  | Return : 'a -> ('p, 'p, 'a) t
  | Bind : ('p, 'q, 'a) t * ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  | Stream : string Stream.stream -> (e, f, unit) t
  | String : string -> (e, f, unit) t
  | Respond : Vif_status.t -> (f, s, unit) t

let bind x fn = Bind (x, fn)
let respond status = Respond status
let return x = Return x
let add ~field value = Add_header (field, value)
let add_unless_exists ~field value = Add_unless_exists (field, value)
let set ~field value = Set_header (field, value)
let rem ~field = Rem_header field
let ( let* ) = bind
let strf fmt = Format.asprintf fmt

module Hdrs = Vif_headers

let compress_string ~headers str =
  match Vif_headers.get headers "content-encoding" with
  | Some "gzip" -> assert false
  | _ -> str

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
      return true
  | `DEFLATE -> return false

let with_stream ?compression:alg req stream =
  match alg with
  | Some alg ->
      let* _ = compression alg req in
      let field = "transfer-encoding" in
      let v = "chunked" in
      let* _ = add_unless_exists ~field v in
      Stream stream
  | None ->
      let field = "transfer-encoding" in
      let v = "chunked" in
      let* _ = add_unless_exists ~field v in
      Stream stream

let with_string ?compression:alg req str =
  match alg with
  | Some alg ->
      let* _ = compression alg req in
      let field = "content-length" in
      let v = string_of_int (String.length str) in
      let* _ = add_unless_exists ~field v in
      String str
  | None ->
      let field = "content-length" in
      let v = string_of_int (String.length str) in
      let* _ = add_unless_exists ~field v in
      String str

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
      let push body str =
        H1.Body.Writer.write_string body str;
        body
      in
      let full _ = false in
      let stop = H1.Body.Writer.close in
      (Sink { init; push; full; stop } : (string, unit) Stream.sink)
  | `V2 reqd ->
      let hdrs = H2.Headers.of_list hdrs in
      let resp = H2.Response.create ~headers:hdrs status in
      let init () = H2.Reqd.respond_with_streaming reqd resp in
      let push body str =
        H2.Body.Writer.write_string body str;
        body
      in
      let full _ = false in
      (* TODO(dinosaure): content-length? *)
      let stop = H2.Body.Writer.close in
      (Sink { init; push; full; stop } : (string, unit) Stream.sink)

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
    | Empty, Stream stream -> (Filled stream, ())
    | Empty, String str ->
        if Vif_request0.version req = 1
        then headers := Vif_headers.add_unless_exists !headers "connection" "close";
        (Filled (Stream.Stream.singleton str), ())
    | Filled stream, Respond status ->
        let headers = !headers in
        let headers, stream =
          match Vif_headers.get headers "content-encoding" with
          | Some "deflate" ->
              let flow = Stream.Flow.deflate () in
              let stream = Stream.Stream.via flow stream in
              let headers = Vif_headers.rem headers "content-length" in
              let headers =
                Vif_headers.add_unless_exists headers "transfer-encoding"
                  "chunked"
              in
              (headers, stream)
          | _ -> (headers, stream)
        in
        let sink = response ~headers status req in
        (Sent, Stream.Stream.into sink stream)
  in
  go s t
