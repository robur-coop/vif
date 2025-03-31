#require "tyxml-ppx" ;;
#require "tyxml" ;;
#require "vif" ;;

open Tyxml ;;

let%html form = {html|
<html>
  <head><title>Vif!</title>
  <body>
    <form method="post" enctype="multipart/form-data" action="login">
      <label for="username">Username:</label>
      <input type="text" name="username" id="username" required />
      <label for="password">Password:</label>
      <input type="password" name="password" id="password" required />
      <label for="age">Age:</label>
      <input type="number" name="age" id="age" value="18" />
      <input type="submit" value="Enter!" />
    </form>
  </body>
</html>
|html} ;;

let form : Tyxml_html.doc = form ;;

let%html apply username = {html|
<html>
  <head><title>Vif!</title></head>
  <body><p>Hello |html} username {html| !</p></body>
</html>
|html} ;;

let apply username age : Tyxml_html.doc =
  let str = Fmt.str "%s (%d years)" username age in
  apply [ Html.txt str ]
;;

open Vif ;;

type credential =
  { username : string
  ; password : string
  ; age : int }
;;

let login req server cfg =
  match Vif.Request.of_multipart_form req with
  | Ok { username; password; age } ->
    Logs.debug (fun m -> m "new user %S" username);
    let* () = Response.with_tyxml req (apply username age) in
    Response.respond `OK
  | _ ->
    let field = "content-type" in
    let* () = Response.add ~field "text/plain; charset=utf-8" in
    let* () = Response.with_string req "Invalid multipart-form\n" in
    Response.respond (`Code 422)
;;

let default req _server () =
  let* () = Response.with_tyxml req form in
  Response.respond `OK
;;

let form =
  let open Vif.Multipart_form in
  let fn username password age =
    { username; password; age } in
  record fn
  |+ field "username" string
  |+ field "password" string
  |+ field "age" int
  |> sealr
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ get (rel /?? nil) --> default
  ; post (m form) (rel / "login" /?? nil) --> login ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
