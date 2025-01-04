#require "miou.unix" ;;
#require "mirage-crypto-rng-miou-unix" ;;
#require "vif" ;;

open Vif

let[@warning "-8"] index (`V1 reqd : Httpcats.Server.reqd) =
  let open H1 in
  let text = "Hello from an OCaml script!" in
  let headers =
    Headers.of_list
      [
        ("content-type", "text/plain; charset=utf-8")
      ; ("content-length", string_of_int (String.length text))
      ]
  in
  let resp = Response.create ~headers `OK in
  Reqd.respond_with_string reqd resp text
;;

let routes =
  let open U in
  let open R in
  [ (rel /?? nil) --> index ]
;;

let () =
  Miou_unix.run @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  Vif.run routes sockaddr;
  Mirage_crypto_rng_miou_unix.kill rng
;;
