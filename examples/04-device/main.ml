#require "vif" ;;

type foo = Foo ;;

let foo =
  let finally Foo = () in
  Vif.D.device ~name:"foo" ~finally [] @@ fun () -> Foo
;;

open Vif ;;

let default req server () =
  let Foo = Vif.S.device foo server in
  let* () = Response.with_string req "ok\n" in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ get (rel /?? nil) --> default ]

let () =
  Miou_unix.run @@ fun () ->
  Vif.run ~devices:Vif.Ds.[ foo ] routes ()
;;
