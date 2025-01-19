type t = {
    request: [ `V1 of H1.Request.t | `V2 of H2.Request.t ]
  ; body: [ `V1 of H1.Body.Reader.t | `V2 of H2.Body.Reader.t ]
}

let of_reqd = function
  | `V1 reqd ->
      let request = `V1 (H1.Reqd.request reqd) in
      let body = `V1 (H1.Reqd.request_body reqd) in
      { request; body }
  | `V2 reqd ->
      let request = `V2 (H2.Reqd.request reqd) in
      let body = `V2 (H2.Reqd.request_body reqd) in
      { request; body }

let target { request; _ } =
  match request with
  | `V1 request -> request.H1.Request.target
  | `V2 request -> request.H2.Request.target

let meth { request; _ } =
  match request with
  | `V1 request -> request.H1.Request.meth
  | `V2 request -> request.H2.Request.meth

let version { request; _ } = match request with `V1 _ -> 1 | `V2 _ -> 2

let headers { request; _ } =
  match request with
  | `V1 request -> H1.Headers.to_list request.H1.Request.headers
  | `V2 request -> H2.Headers.to_list request.H2.Request.headers

let to_string ~schedule ~close body =
  let buf = Buffer.create 0x7ff in
  let c = Miou.Computation.create () in
  let rec on_eof () =
    close body;
    assert (Miou.Computation.try_return c (Buffer.contents buf))
  and on_read bstr ~off ~len =
    Buffer.add_string buf (Bigstringaf.substring bstr ~off ~len);
    schedule body ~on_eof ~on_read
  in
  schedule body ~on_eof ~on_read;
  Miou.Computation.await_exn c

let to_stream ~schedule ~close body =
  let stream = Stream.create 0x7ff in
  let rec on_eof () = close body; Stream.close stream
  and on_read bstr ~off ~len =
    Stream.put stream (Bigstringaf.substring bstr ~off ~len);
    schedule body ~on_eof ~on_read
  in
  schedule body ~on_eof ~on_read;
  stream

let to_string { body; _ } =
  match body with
  | `V1 body ->
      to_string ~schedule:H1.Body.Reader.schedule_read
        ~close:H1.Body.Reader.close body
  | `V2 body ->
      to_string ~schedule:H2.Body.Reader.schedule_read
        ~close:H2.Body.Reader.close body

let to_stream { body; _ } =
  match body with
  | `V1 body ->
      to_stream ~schedule:H1.Body.Reader.schedule_read
        ~close:H1.Body.Reader.close body
  | `V2 body ->
      to_stream ~schedule:H2.Body.Reader.schedule_read
        ~close:H2.Body.Reader.close body
