#require "vif" ;;
#require "jwto" ;;

type user =
  { username : string }
;;

type cfg =
  { secret : string }
;;

let jwt = Vif.Middlewares.v ~name:"jwt" @@ fun req target server { secret } ->
  match Vif.Cookie.get server req ~name:"vif-token" with
  | Error err -> None
  | Ok token ->
      let ( let* ) = Option.bind in
      let* token = Result.to_option (Jwto.decode_and_verify secret token) in
      let* username = List.assoc_opt "username" (Jwto.get_payload token) in
      Some { username }
;;

type credential =
  { username : string
  ; password : string }
;;

let credential =
  let open Jsont in
  let username = Object.mem "username" string in
  let password = Object.mem "password" string in
  let fn username password = { username; password } in
  Object.map fn
  |> username
  |> password
  |> Object.finish
;;

let form =
  let open Vif.Multipart_form in
  let fn username password =
    { username; password } in
  record fn
  |+ field "username" string
  |+ field "password" string
  |> sealr
;;

let users =
  [ "dinosaure", "foo" ]
;;

let login req server { secret } { username; password } =
  let open Vif.Response.Syntax in
  match List.assoc_opt username users with
  | Some p' when password = p' ->
      let token = Jwto.encode HS512 secret [ "username", username ] in
      let token = Result.get_ok token in
      let* () = Vif.Cookie.set ~name:"vif-token" server req token in
      let field = "content-type" in
      let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Authenticated!\n" in
      Vif.Response.respond `OK
  | _ ->
      let field = "content-type" in
      let* () = Vif.Response.add ~field "text/plain; charset= utf-8" in
      let* () = Vif.Response.with_string req "Bad credentials\n" in
      Vif.Response.respond `Unauthorized

let login_by_json req server cfg =
  let open Vif.Response.Syntax in
  match Vif.Request.of_json req with
  | Ok credential -> login req server cfg credential
  | Error _ ->
      let field = "content-type" in
      let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Invalid JSON\n" in
      Vif.Response.respond (`Code 422)
;;

let login_by_form req server cfg =
  let open Vif.Response.Syntax in
  match Vif.Request.of_multipart_form req with
  | Ok credential -> login req server cfg credential
  | Error _ ->
      let field = "content-type" in
      let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Invalid multipart-form\n" in
      Vif.Response.respond (`Code 422)
;;

let default req server _cfg =
  let open Vif.Response.Syntax in
  match Vif.Request.get jwt req with
  | None ->
      let field = "content-type" in
      let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Unauthorized place\n" in
      Vif.Response.respond `Unauthorized
  | Some { username } ->
      let field = "content-type" in
      let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
      let str = Fmt.str "Connected as %S\n" username in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (m form) (rel / "login" /?? nil) --> login_by_form
  ; post (json_encoding credential) (rel / "login" /?? nil) --> login_by_json
  ; get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  let secret = "deadbeef" in
  Vif.run ~middlewares:Vif.Middlewares.[ jwt ] routes { secret }
;;
