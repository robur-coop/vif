#require "vif" ;;

open Vif ;;

let cat req server _ =
  let src = Request.stream req in
  let field = "content-type" in
  let* () = Response.add ~field "application/octet-stream" in
  let* () = Response.with_stream req src in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ post any (rel /?? nil) --> cat ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
