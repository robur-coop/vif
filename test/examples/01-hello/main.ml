#require "vif" ;;

open Vif ;;

let default req _server () =
  let str = "Hello World!\n" in
  let* () = Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Response.with_string req str in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
