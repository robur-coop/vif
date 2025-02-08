let src = Logs.Src.create "vif.response"

module Log = (val Logs.src_log src : Logs.LOG)

type t = Response

let strf fmt = Format.asprintf fmt

module Hdrs = Vif_headers

let compress_string ~headers str =
  match List.assoc_opt "content-encoding" headers with
  | Some "gzip" -> assert false
  | _ -> str

let with_string server ?headers:(hdrs = []) status str =
  match Vif_s.reqd server with
  | `V1 reqd ->
      let length = strf "%d" (String.length str) in
      let hdrs = Hdrs.add_unless_exists hdrs "content-length" length in
      let hdrs = Hdrs.add_unless_exists hdrs "connection" "close" in
      let str = compress_string ~headers:hdrs str in
      let hdrs = H1.Headers.of_list hdrs in
      let status =
        match status with
        | #H1.Status.t as status -> status
        | status ->
            Log.err (fun m -> m "Invalid status: %a" H2.Status.pp_hum status);
            invalid_arg "Response.with_string: invalid status"
      in
      let resp = H1.Response.create ~headers:hdrs status in
      H1.Reqd.respond_with_string reqd resp str;
      Response
  | `V2 reqd ->
      let length = strf "%d" (String.length str) in
      let hdrs = Hdrs.add_unless_exists hdrs "content-length" length in
      let str = compress_string ~headers:hdrs str in
      let hdrs = H2.Headers.of_list hdrs in
      let resp = H2.Response.create ~headers:hdrs status in
      H2.Reqd.respond_with_string reqd resp str;
      Response

let with_stream ?compression server ?headers status stream =
  let headers, stream =
    match compression with
    | None -> (headers, stream)
    | Some `DEFLATE ->
        let headers =
          match headers with
          | None -> Some [ ("content-encoding", "deflate") ]
          | Some hdrs ->
              Vif_headers.add_unless_exists hdrs "content-encoding" "deflate"
              |> Option.some
        in
        (headers, Stream.Stream.via (Stream.Flow.deflate ()) stream)
  in
  let sink = Stream.Sink.response ?headers status server in
  Stream.Stream.into sink stream;
  Response
