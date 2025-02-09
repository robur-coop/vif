let src = Logs.Src.create "vif.response"

module Log = (val Logs.src_log src : Logs.LOG)

type t = Response

let strf fmt = Format.asprintf fmt

module Hdrs = Vif_headers

let compress_string ~headers str =
  match List.assoc_opt "content-encoding" headers with
  | Some "gzip" -> assert false
  | _ -> str

let can_compress alg server =
  match Vif_s.reqd server with
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

let with_stream ?compression server ?headers status stream =
  let headers, stream =
    match compression with
    | Some `DEFLATE when can_compress "deflate" server ->
        let headers =
          match headers with
          | None -> Some [ ("content-encoding", "deflate") ]
          | Some hdrs ->
              Vif_headers.add_unless_exists hdrs "content-encoding" "deflate"
              |> Option.some
        in
        (headers, Stream.Stream.via (Stream.Flow.deflate ()) stream)
    | _ -> (headers, stream)
  in
  let sink = Stream.Sink.response ?headers status server in
  Stream.Stream.into sink stream;
  Response

let with_string ?compression server ?headers status str =
  with_stream ?compression server ?headers status (Stream.Stream.singleton str)
