#require "mirage-crypto-rng-miou-unix" ;;
#require "vif" ;;

let rng =
  let open Mirage_crypto_rng_miou_unix in
  let finally = kill in
  Vif.D.device ~name:"rng" ~finally [] @@ fun () ->
  initialize (module Pfortuna)
;;

type foo = Foo ;;

let foo =
  let finally Foo = () in
  Vif.D.device ~name:"foo" ~finally [] @@ fun () ->
  Foo
;;

let default req target server () =
  let _rng = Vif.S.device rng server in
  let Foo = Vif.S.device foo server in
  Vif.Response.with_string server `OK "ok\n"
;;

let () =
  Miou_unix.run @@ fun () ->
    Vif.run ~default ~devices:Vif.Ds.[ rng; foo ] [] ()
;;
