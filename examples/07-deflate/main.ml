#require "vif" ;;

let default req target server () =
  let stream = Vif.Stream.Stream.singleton "Hello World!\n" in
  Vif.Response.with_stream ~compression:`DEFLATE server `OK stream
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~default [] () ;;
