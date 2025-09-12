type body = |
type response = unit

exception Client_error of Httpcats.error

type resolver =
  [ `Happy of Happy_eyeballs_miou_unix.t | `User of Httpcats.resolver | `System ]

let request ?config ?tls_config ?authenticator ?meth ?headers ?body:_
    ?max_redirect ?follow_redirect ?resolver t =
  let fn _meta _request _response a _chunk = a in
  let fn uri =
    let uri = "https://" ^ uri in
    (* TODO *)
    let res =
      Httpcats.request ?config ?tls_config ?authenticator ?meth ?headers
        ?max_redirect ?follow_redirect ?resolver ~fn ~uri ()
    in
    match res with
    | Ok (_response, ()) -> ()
    | Error (#Httpcats.error as err) -> raise (Client_error err)
  in
  Vif_core.Uri.keval t fn
