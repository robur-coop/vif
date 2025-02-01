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

let with_stream server ?headers:(hdrs = []) status fn =
  match Vif_s.reqd server with
  | `V1 reqd ->
      let hdrs = Hdrs.add_unless_exists hdrs "transfer-encoding" "chunked" in
      let hdrs = H1.Headers.of_list hdrs in
      let status =
        match status with
        | #H1.Status.t as status -> status
        | _ -> invalid_arg "Response.with_string: invalid status"
      in
      let resp = H1.Response.create ~headers:hdrs status in
      let stream = Stream.create 0x7ff in
      let body = H1.Reqd.respond_with_streaming reqd resp in
      let res0 = Miou.Ownership.create ~finally:H1.Body.Writer.close body in
      let res1 = Miou.Ownership.create ~finally:Stream.close stream in
      let rec send stream body res =
        match Stream.get stream with
        | Some str ->
            H1.Body.Writer.write_string body str;
            send stream body res
        | None -> H1.Body.Writer.close body; Miou.Ownership.disown res
      in
      let prm0 = Miou.async ~give:[ res0 ] @@ fun () -> send stream body res0 in
      let prm1 =
        Miou.async ~give:[ res1 ] @@ fun () ->
        let () = fn stream in
        Stream.close stream; Miou.Ownership.disown res1
      in
      Miou.await_all [ prm0; prm1 ]
      |> List.iter (function Ok () -> () | Error exn -> raise exn);
      Response
  | `V2 _ -> assert false
