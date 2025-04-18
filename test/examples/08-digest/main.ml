#require "vif" ;;
#require "digestif.c" ;;

open Vif ;;

let sha1 =
  let open S in
  let init () = Digestif.SHA1.empty in
  let push ctx str = Digestif.SHA1.feed_string ctx str in
  let full = Fun.const false in
  let stop = Digestif.SHA1.get in
  Sink { init; push; full; stop }
;;

let default req server () =
  let from = Request.source req in
  let hash, src = S.Stream.run ~from ~via:S.Flow.identity ~into:sha1 in
  Option.iter S.Source.dispose src;
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  let* () = Response.with_string req (Digestif.SHA1.to_hex hash) in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ post any (rel /?? nil) --> default ]

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
