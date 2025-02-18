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
  | Error err ->
      Logs.err (fun m -> m "jwt: %a" Cookie.pp_error err);
      None
  | Ok token ->
      Logs.debug (fun m -> m "Token found: %S" token);
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

let users =
  [ "dinosaure", "foo" ]
;;

let login req server { secret }=
  match Vif.Request.of_json req with
  | Ok { username; password } ->
      begin match List.assoc_opt username users with
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
          Response.respond `Unauthorized end
  | Error _ ->
      let field = "content-type" in
      let* () = Response.add ~field "text/plain; charset=utf-8" in
      let* () = Response.with_string req "Invalid JSON\n" in
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
  let open Vif.Content_type in
  [ post (json_encoding credential) (rel / "login" /?? nil) --> login
  ; get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  let secret = "deadbeef" in
  Vif.run ~middlewares:Ms.[ jwt ] routes { secret }
;;
