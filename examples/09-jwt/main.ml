#require "vif" ;;
#require "jwto" ;;

open Vif ;;

type user =
  { username : string }
;;

type cfg =
  { secret : string }
;;

let jwt = Vif.Ms.make ~name:"jwt" @@ fun req target server { secret } ->
  Logs.debug (fun m -> m "Search vif-token cookie");
  match Cookie.get server req ~name:"vif-token" with
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
  let open Json_encoding in
  let username = req "username" string in
  let password = req "password" string in
  let credential = obj2 username password in
  let prj { username; password } =
    (username, password) in
  let inj (username, password) =
    { username; password } in
  conv prj inj credential
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
  match List.assoc_opt username users with
  | Some p' when password = p' ->
      let token = Jwto.encode HS512 secret [ "username", username ] in
      let token = Result.get_ok token in
      let* () = Vif.Cookie.set ~name:"vif-token" server req token in
      let field = "content-type" in
      let* () = Response.add ~field "text/plain; charset=utf-8" in
      let* () = Response.with_string req "Authenticated!\n" in
      Response.respond `OK
  | _ ->
      let field = "content-type" in
      let* () = Response.add ~field "text/plain; charset= utf-8" in
      let* () = Response.with_string req "Bad credentials\n" in
      Response.respond `Unauthorized

let login_by_json req server cfg =
  match Vif.Request.of_json req with
  | Ok credential -> login req server cfg credential
  | Error _ ->
      let field = "content-type" in
      let* () = Response.add ~field "text/plain; charset=utf-8" in
      let* () = Response.with_string req "Invalid JSON\n" in
      Response.respond (`Code 422)
;;

let login_by_form req server cfg =
  match Vif.Request.of_multipart_form req with
  | Ok credential -> login req server cfg credential
  | Error _ ->
      let field = "content-type" in
      let* () = Response.add ~field "text/plain; charset=utf-8" in
      let* () = Response.with_string req "Invalid multipart-form\n" in
      Response.respond (`Code 422)
;;

let default req server _cfg =
  match Request.get jwt req with
  | None ->
      let field = "content-type" in
      let* () = Response.add ~field "text/plain; charset=utf-8" in
      let* () = Response.with_string req "Unauthorized place\n" in
      Response.respond `Unauthorized
  | Some { username } ->
      let field = "content-type" in
      let* () = Response.add ~field "text/plain; charset=utf-8" in
      let str = Fmt.str "Connected as %S\n" username in
      let* () = Response.with_string req str in
      Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ post (m form) (rel / "login" /?? nil) --> login_by_form
  ; post (json_encoding credential) (rel / "login" /?? nil) --> login_by_json
  ; get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  let secret = "deadbeef" in
  Vif.run ~middlewares:Ms.[ jwt ] routes { secret }
;;
