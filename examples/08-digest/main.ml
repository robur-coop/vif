#require "vif" ;;
#require "digestif.c" ;;

open Vif ;;

let sha1 =
  let open Stream in
  let init () = Digestif.SHA1.empty in
  let push ctx str = Digestif.SHA1.feed_string ctx str in
  let full = Fun.const false in
  let stop = Digestif.SHA1.get in
  Sink { init; push; full; stop }
;;

let default req target server () =
  let stream = Request.stream req in
  let hash = Stream.Stream.into sha1 stream in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  let* () = Response.with_string req (Digestif.SHA1.to_hex hash) in
  Response.respond `OK
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run ~default [] () ;;
