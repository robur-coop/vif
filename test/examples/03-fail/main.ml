#require "vif" ;;

exception Foo ;;

let () = Printexc.register_printer @@ function
  | Foo -> Some "Foo"
  | _ -> None ;;

let default req server () = raise Foo ;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
