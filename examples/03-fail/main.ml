#require "vif" ;;

exception Foo ;;

let () = Printexc.register_printer @@ function
  | Foo -> Some "Foo"
  | _ -> None ;;

let default req server () = raise Foo ;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
