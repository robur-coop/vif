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
  Vif.D.device ~name:"caqti" ~finally [] @@ fun { sw; uri } ->
  match Caqti_miou_unix.connect ~sw uri with
  | Ok conn -> conn
  | Error err ->
      Logs.err (fun m -> m "%a" Caqti_error.pp err);
      Fmt.failwith "%a" Caqti_error.pp err
;;

open Vif ;;
open Caqti_request.Infix ;;

let add req n server _cfg =
  let (module Conn) = Vif.S.device caqti server in
  let sql = Caqti_type.(int ->. unit) ("INSERT INTO t (f) VALUES (?)") in
  match Conn.exec sql n with
  | Ok () ->
    let str = (Fmt.str "%d Added\n" n) in
    let field = "content-type" in
    let* () = Response.add ~field "text/plain; charset=utf-8" in
    let* () = Response.with_string req str in
    Response.respond `OK
  | Error err ->
    let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
    let* () = Response.with_string req str in
    Response.respond `Internal_server_error
;;

let list req server _cfg =
  let (module Conn) = Vif.S.device caqti server in
  let sql = Caqti_type.(unit ->* int) ("SELECT f FROM t") in
  match Conn.collect_list sql () with
  | Ok lst ->
    let str = Fmt.str "%a" Fmt.(Dump.list int) lst in
    let field = "content-type" in
    let* () = Response.add ~field "text/plain; charset=utf-8" in
    let* () = Response.with_string req str in
    Response.respond `OK
  | Error err ->
    let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
    let* () = Response.with_string req str in
    Response.respond `Internal_server_error
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ post any (rel / "add" /% Tyre.int /?? nil) --> add
  ; get (rel /?? nil) --> list ]

let () =
  Miou_unix.run @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  let uri = Uri.of_string "sqlite3:foo.sqlite?create=false" in
  let cfg = { sw; uri } in
  Vif.run ~devices:Vif.Ds.[ caqti ] routes cfg
;;
