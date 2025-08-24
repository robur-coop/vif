let form_elem = Brr.El.find_first_by_selector (Jstr.v "#send") |> Option.get

let send_message ev_submit =
  let open Fut.Syntax in
  let _ = Jv.call (Brr.Ev.to_jv ev_submit) "preventDefault" [||] in
  let form_elem = Brr_io.Form.of_el form_elem in
  let form_data = Brr_io.Form.Data.of_form form_elem in
  let body = Brr_io.Fetch.Body.of_form_data form_data
  and credentials = Brr_io.Fetch.Request.Credentials.same_origin in
  let init = Brr_io.Fetch.Request.init
    ~body ~credentials ~method':(Jstr.v "POST") () in
  let req = Brr_io.Fetch.Request.v ~init (Jstr.v "http://localhost:8080/send") in
  Fut.await begin
    let* result = Brr_io.Fetch.request req in
    match result with
    | Ok resp when Brr_io.Fetch.Response.ok resp -> Fut.return ()
    | Ok _resp -> print_endline "Error!"; Fut.return ()
    | Error _ -> print_endline "Error!"; Fut.return ()
  end @@ Fun.id

let on_message ev =
  let msg = Jv.Jstr.get (Brr.Ev.to_jv ev) "data" in
  let div = Brr.El.(div [txt msg]) in
  Brr.El.append_children (Brr.Document.body Brr.G.document) [div]

let () =
  let socket = Brr_io.Websocket.create (Jstr.v "http://localhost:8080/chat") in
  let target = Brr_io.Websocket.as_target socket in
  let event = Brr.Ev.Type.create (Jstr.v "message") in
  ignore (Brr.Ev.listen event on_message target);
  let target = Brr.El.as_target form_elem in
  ignore (Brr.Ev.listen Brr_io.Form.Ev.submit send_message target)
