#require "vif" ;;

open Vif ;;

let cat req server _ =
  let src = Request.source req in
  let field = "content-type" in
  let* () = Response.add ~field "application/octet-stream" in
  let* () = Response.with_source req src in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ post any (rel /?? nil) --> cat ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
