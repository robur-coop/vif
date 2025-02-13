#require "vif" ;;
#require "fmt" ;;

let counter = Atomic.make 0 ;;

open Vif ;;

let default req target _server () =
  let v = Atomic.fetch_and_add counter 1 in
  let str = Fmt.str "%d request(s)\n" (succ v) in
  let* () = Response.with_string req str in
  Response.respond `OK
;;

let () = Miou_unix.run @@ fun () -> Vif.run ~default [] () ;;
