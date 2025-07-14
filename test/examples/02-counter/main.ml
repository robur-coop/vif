#require "vif" ;;
#require "fmt" ;;

let counter = Atomic.make 0 ;;

open Vif ;;

let default req _server () =
  let open Response.Syntax in
  let v = Atomic.fetch_and_add counter 1 in
  let str = Fmt.str "%d request(s)\n" (succ v) in
  let* () = Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Response.with_string req str in
  Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ get (rel /?? nil) --> default ]

let () = Miou_unix.run @@ fun () ->
  Vif.run routes () ;;
