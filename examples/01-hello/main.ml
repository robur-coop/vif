#require "vif" ;;

open Vif ;;

let default req target _server () =
  let* () = Response.with_string req "Hello World!\n" in
  Response.respond `OK
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~default [] () ;;
