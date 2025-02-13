#require "vif" ;;

type foo = Foo ;;

let foo =
  let finally Foo = () in
  Vif.D.device ~name:"foo" ~finally [] @@ fun () -> Foo
;;

open Vif ;;

let default req target server () =
  let Foo = Vif.S.device foo server in
  let* () = Response.with_string req "ok\n" in
  Response.respond `OK
;;

let () =
  Miou_unix.run @@ fun () -> Vif.run ~default ~devices:Vif.Ds.[ foo ] [] ()
;;
