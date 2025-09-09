#require "vif" ;;

let greeting req _server () =
  let open Vif.Response.Syntax in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  let greeting =
    match Vif.Request.of_multipart_form req with
    | Ok greeting -> greeting
    | Error _ -> "stranger"
  in
  let* () =
    Format.kasprintf (Vif.Response.with_string req)
      "Hello POST %s!\n" greeting
  in
  Vif.Response.respond `OK

let greeting' req _server () =
  let open Vif.Response.Syntax in
  let greeting =
    Vif.Queries.get req "greeting"
    |> String.concat ","
  in
  let greeting = if String.(equal empty) greeting then "stranger" else greeting in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  let* () =
    Format.kasprintf (Vif.Response.with_string req)
      "Hello GET %s!\n" greeting
  in
  Vif.Response.respond `OK

let form =
  let open Vif.Multipart_form in
  record Fun.id
  |+ field "greeting" string
  |> sealr
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [ get (rel /?? any) --> greeting'
  ; post (Vif.Type.m form) (rel /?? nil) --> greeting ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
