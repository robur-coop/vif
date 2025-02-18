#require "vif" ;;

open Vif ;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~handlers:[ Handler.static ] [] ()
;;
