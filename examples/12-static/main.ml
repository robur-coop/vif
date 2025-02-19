#require "vif" ;;

open Vif ;;

let default req server _ =
  Response.with_file ~compression:`DEFLATE req (Fpath.v "index.html")
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~handlers:[ Handler.static ] routes ()
;;
