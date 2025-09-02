#require "vif" ;;

let deflate req server () =
  let open Vif.Response.Syntax in
  let* () = Vif.Response.with_string ~compression:`DEFLATE req "Hello World!\n" in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  Vif.Response.respond `OK
;;

let gzip req server () =
  let open Vif.Response.Syntax in
  let* () = Vif.Response.with_string ~compression:`Gzip req "Hello World!\n" in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ get (rel / "deflate" /?? nil) --> deflate
  ; get (rel / "gzip" /?? nil) --> gzip ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
