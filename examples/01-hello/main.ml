#require "vif" ;;

let default req target server () =
  Vif.Response.with_string server `OK "Hello World!\n"
;;

let () = Miou_unix.run @@ fun () -> Vif.run ~default [] () ;;
