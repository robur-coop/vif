#require "vif" ;;

open Vif ;;

let default req server () =
  let* () = Response.with_string ~compression:`DEFLATE req "Hello World!\n" in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ get (rel /?? nil) --> default ]

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
