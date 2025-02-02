#require "vif" ;;
#require "fmt" ;;

let counter = Atomic.make 0 ;;

let default req target server () =
  let v = Atomic.fetch_and_add counter 1 in
  Vif.Response.with_string server `OK (Fmt.str "%d request(s)\n" (succ v))
;;

let () = Miou_unix.run @@ fun () -> Vif.run ~default [] () ;;
