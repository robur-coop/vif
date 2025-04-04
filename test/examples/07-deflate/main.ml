#require "vif" ;;

open Vif ;;

let deflate req server () =
  let* () = Response.with_string ~compression:`DEFLATE req "Hello World!\n" in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  Response.respond `OK
;;

let gzip req server () =
  let* () = Response.with_string ~compression:`Gzip req "Hello World!\n" in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ get (rel / "deflate" /?? nil) --> deflate
  ; get (rel / "gzip" /?? nil) --> gzip ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
