#require "vif" ;;
#require "digestif.c" ;;

let sha1 =
  let open Vif.Stream in
  let init () = Digestif.SHA1.empty in
  let push ctx str = Digestif.SHA1.feed_string ctx str in
  let full = Fun.const false in
  let stop = Digestif.SHA1.get in
  Sink { init; push; full; stop }
;;

let default req server () =
  let from = Vif.Request.source req in
  let hash, src = Vif.Stream.Stream.run ~from ~via:Vif.Stream.Flow.identity ~into:sha1 in
  Option.iter Vif.Stream.Source.dispose src;
  let open Vif.Response.Syntax in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req (Digestif.SHA1.to_hex hash) in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post any (rel /?? nil) --> default ]

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
