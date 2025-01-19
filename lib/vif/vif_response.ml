type t = { response: Httpcats.response; compress: [ `GZip | `DEFLATE ] option }

let strf fmt = Format.asprintf fmt

let with_string server ?(headers = []) status str =
  match Vif_s.reqd server with
  | `V1 reqd ->
      let headers =
        Vif_headers.add_unless_exists headers "content-length"
          (strf "%d" (String.length str))
      in
      let headers = H1.Headers.of_list headers in
      let status =
        match status with
        | #H1.Status.t as status -> status
        | _ -> invalid_arg "Response.with_string: invalid status"
      in
      let resp = H1.Response.create ~headers status in
      H1.Reqd.respond_with_string reqd resp str
  | `V2 reqd ->
      let headers =
        Vif_headers.add_unless_exists headers "content-length"
          (strf "%d" (String.length str))
      in
      let headers = H2.Headers.of_list headers in
      let resp = H2.Response.create ~headers status in
      H2.Reqd.respond_with_string reqd resp str

let with_stream server ?(headers = []) status fn =
  match Vif_s.reqd server with
  | `V1 reqd ->
      let headers =
        Vif_headers.add_unless_exists headers "transfer-encoding" "chunked"
      in
      let headers = H1.Headers.of_list headers in
      let status =
        match status with
        | #H1.Status.t as status -> status
        | _ -> invalid_arg "Response.with_string: invalid status"
      in
      let resp = H1.Response.create ~headers status in
      let stream = Stream.create 0x7ff in
      let body = H1.Reqd.respond_with_streaming reqd resp in
      let res0 =
        let finally = H1.Body.Writer.close in
        Miou.Ownership.create ~finally body
      in
      let res1 =
        let finally = Stream.close in
        Miou.Ownership.create ~finally stream
      in
      let prm0 =
        Miou.async ~give:[ res0 ] @@ fun () ->
        let rec go () =
          match Stream.get stream with
          | Some str ->
              let () = H1.Body.Writer.write_string body str in
              go ()
          | None -> H1.Body.Writer.close body; Miou.Ownership.disown res0
        in
        go ()
      in
      let prm1 =
        Miou.async ~give:[ res1 ] @@ fun () ->
        let () = fn stream in
        Stream.close stream; Miou.Ownership.disown res1
      in
      Miou.await_all [ prm0; prm1 ]
      |> List.iter (function Ok () -> () | Error exn -> raise exn)
  | `V2 _ -> assert false
