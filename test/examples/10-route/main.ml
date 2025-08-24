#require "vif" ;;

let hello req name server _ =
  let open Vif.Response.Syntax in
  let str = Fmt.str "Hello, %S!\n" name in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req str in
  Vif.Response.respond `OK
;;

let default req server _cfg =
  let open Vif.Response.Syntax in
  let str = Fmt.str "Hello World!\n" in
  let field = "content-type" in
  let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req str in
  Vif.Response.respond `OK
;;

let query req foo _server _cfg =
  let open Vif.Response.Syntax in
  match Vif.Queries.get req "foo" with
  | [] ->
    let str = "Foo not found\n" in
    let field = "content-type" in
    let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `Bad_request
  | v :: _ ->
    let str = Fmt.str "foo: %d (%S)\n" foo v in
    let field = "content-type" in
    let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [ get (rel / "echo" /% string `Path /?? nil) --> hello
  ; get (rel / "query" /?? ("foo", int) ** any) --> query
  ; get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
