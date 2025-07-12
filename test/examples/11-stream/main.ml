#require "vif" ;;

open Vif ;;

let cat req server _ =
  let open Response.Syntax in
  let src = Request.source req in
  let field = "content-type" in
  let* () = Response.add ~field "application/octet-stream" in
  let* () = Response.with_source req src in
  Response.respond `OK
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
