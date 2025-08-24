#require "vif" ;;

let default req _server () =
  let open Vif.Response.Syntax in
  let str = "Hello World!\n" in
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req str in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
