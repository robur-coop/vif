#require "vif" ;;

open Vif ;;

let hello req name server _ =
  let str = Fmt.str "Hello, %S!\n" name in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  let* () = Response.with_string req str in
  Response.respond `OK
;;

let default req server _cfg =
  let str = Fmt.str "Hello World!\n" in
  let field = "content-type" in
  let* () = Response.add ~field "text/plain; charset=utf-8" in
  let* () = Response.with_string req str in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ get (rel / "echo" /% string /?? nil) --> hello
  ; get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
