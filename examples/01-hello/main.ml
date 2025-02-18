#require "vif" ;;

open Vif ;;

let default req _server () =
  let* () = Response.with_string req "Hello World!\n" in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes () ;;
