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

let default req server () =
  let from = Request.source req in
  let hash, src = Stream.Stream.run ~from ~via:Stream.Flow.identity ~into:sha1 in
  Option.iter Stream.Source.dispose src;
  let open Response.Syntax in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  let* () = Response.with_string req (Digestif.SHA1.to_hex hash) in
  Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post any (rel /?? nil) --> default ]

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
