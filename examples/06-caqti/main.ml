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

open Caqti_request.Infix ;;

let add req n server _cfg =
  let (module Conn) = Vif.S.device caqti server in
  let req = Caqti_type.(int ->. unit) ("INSERT INTO t (f) VALUES (?)") in
  match Conn.exec req n with
  | Ok () ->
    Vif.Response.with_string server `OK (Fmt.str "%d Added\n" n)
  | Error err ->
    let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
    Vif.Response.with_string server `Internal_server_error str
;;

let list req server _cfg =
  let (module Conn) = Vif.S.device caqti server in
  let req = Caqti_type.(unit ->* int) ("SELECT f FROM t") in
  match Conn.collect_list req () with
  | Ok lst ->
    let str = Fmt.str "%a" Fmt.(Dump.list int) lst in
    Vif.Response.with_string server `OK str
  | Error err ->
    let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
    Vif.Response.with_string server `Internal_server_error str
;;

let default req target server _ =
  let str = Fmt.str "%s not found\n" target in
  Vif.Response.with_string server `Not_found str
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.Content_type in
  [ post any (rel / "add" /% Tyre.int /?? nil) --> add
  ; get (rel /?? nil) --> list ]

let () =
  Miou_unix.run @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  let uri = Uri.of_string "sqlite3:foo.sqlite?create=false" in
  let cfg = { sw; uri } in
  Vif.run ~default ~devices:Vif.Ds.[ caqti ] routes cfg
;;
