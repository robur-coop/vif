#require "caqti-miou" ;;
#require "caqti-miou.unix" ;;
#require "caqti-driver-sqlite3" ;;
#require "vif" ;;

type cfg =
  { sw : Caqti_miou.Switch.t
  ; uri : Uri.t }
;;

let caqti =
  let finally (module Conn : Caqti_miou.CONNECTION) = Conn.disconnect () in
  Vif.Device.v ~name:"caqti" ~finally [] @@ fun { sw; uri } ->
  match Caqti_miou_unix.connect ~sw uri with
  | Ok conn -> conn
  | Error err ->
      Logs.err (fun m -> m "%a" Caqti_error.pp err);
      Fmt.failwith "%a" Caqti_error.pp err
;;

open Caqti_request.Infix ;;

let add req n server _cfg =
  let (module Conn) = Vif.Server.device caqti server in
  let sql = Caqti_type.(int ->. unit) ("INSERT INTO t (f) VALUES (?)") in
  let open Vif.Response.Syntax in
  match Conn.exec sql n with
  | Ok () ->
    let str = (Fmt.str "%d Added\n" n) in
    let field = "content-type" in
    let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | Error err ->
    let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `Internal_server_error
;;

let list req server _cfg =
  let (module Conn) = Vif.Server.device caqti server in
  let sql = Caqti_type.(unit ->* int) ("SELECT f FROM t") in
  let open Vif.Response.Syntax in
  match Conn.collect_list sql () with
  | Ok lst ->
    let str = Fmt.str "%a" Fmt.(Dump.list int) lst in
    let field = "content-type" in
    let* () = Vif.Response.add ~field "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | Error err ->
    let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `Internal_server_error
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post any (rel / "add" /% int /?? nil) --> add
  ; get (rel /?? nil) --> list ]

let () =
  Miou_unix.run @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  let uri = Uri.of_string "sqlite3:foo.sqlite?create=false" in
  let cfg = { sw; uri } in
  Vif.run ~devices:Vif.Devices.[ caqti ] routes cfg
;;
