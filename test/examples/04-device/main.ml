#require "vif" ;;

type foo = Foo ;;

let foo =
  let finally Foo = () in
  Vif.Device.v ~name:"foo" ~finally [] @@ fun () -> Foo
;;

open Vif ;;

let default req server () =
  let open Response.Syntax in
  let Foo = Vif.Server.device foo server in
  let* () = Response.with_string req "ok\n" in
  Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ get (rel /?? nil) --> default ]

let () =
  Miou_unix.run @@ fun () ->
  Vif.run ~devices:Vif.Devices.[ foo ] routes ()
;;
