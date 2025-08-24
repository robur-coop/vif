#require "vif" ;;

let cat req server _ =
  let open Vif.Response.Syntax in
  let src = Vif.Request.source req in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "application/octet-stream" in
  let* () = Vif.Response.with_source req src in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post any (rel /?? nil) --> cat ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
