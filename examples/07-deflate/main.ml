#require "vif" ;;

open Vif ;;

let default req target server () =
  let* () = Response.with_string ~compression:`DEFLATE req "Hello World!\n" in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  Response.respond `OK
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~default [] () ;;
