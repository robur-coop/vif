type credentials =
  { username : string
  ; password : string }

type env =
  { secret : string
  ; sw : Caqti_miou.Switch.t
  ; uri : Uri.t }

type metadata =
  { username : string }

type user =
  { uid : int
  ; username : string 
  ; password : Digestif.SHA256.t }

let[@alert "-caqti_unstable"] user =
  let open Caqti_template.Row_type in
  let sha256 =
    let encode hash = Ok (Digestif.SHA256.to_hex hash) in
    let decode hex = Ok (Digestif.SHA256.of_hex hex) in
    custom ~encode ~decode string in
  product (fun uid username password -> { uid; username; password })
  @@ proj int (fun (t : user) -> t.uid)
  @@ proj string (fun (t : user) -> t.username)
  @@ proj sha256 (fun (t : user) -> t.password)
  @@ proj_end

let jwt =
  Vif.Middlewares.v ~name:"jwt" @@ fun req _target server { secret; _ } ->
  match Vif.Cookie.get server req ~name:"jwt" with
  | Error _err -> None
  | Ok token ->
      let ( let* ) = Option.bind in
      let token = Jwto.decode_and_verify secret token in
      let* token = Result.to_option token in
      let* username = List.assoc_opt "username" (Jwto.get_payload token) in
      Some { username }

let caqti =
  let finally pool = Caqti_miou_unix.Pool.drain pool in
  Vif.Device.v ~name:"caqti" ~finally [] @@ fun { sw; uri; _ } ->
  match Caqti_miou_unix.connect_pool ~sw uri with
  | Error err -> Fmt.failwith "%a" Caqti_error.pp err
  | Ok pool -> pool

let index username : Tyxml_html.doc =
  let open Tyxml in
  let index username =
    Html.html
      (Html.head (Html.title (Html.txt "My Vif Website!")) [])
      (Html.body [ Html.p [ Html.txt "Hello "; username ] ]) in
  index (Tyxml.Html.txt username)

let index req _server _env =
  let open Vif.Response.Syntax in
  match Vif.Request.get jwt req with
  | None ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Some { username } ->
    let* () = Vif.Response.with_tyxml req (index username) in
    Vif.Response.respond `OK

let login_form =
  let open Vif.Multipart_form in
  let fn username password =
    { username; password } in
  record fn
  |+ field "username" string
  |+ field "password" string
  |> sealr

let login server (c : credentials) =
  let pool = Vif.Server.device caqti server in
  let sql =
    let open Caqti_request.Infix in
    Caqti_type.(string ->* user)
      "SELECT * FROM users WHERE username = ?" in
  let fn (module Conn : Caqti_miou.CONNECTION) = Conn.collect_list sql c.username in
  match Caqti_miou_unix.Pool.use fn pool with
  | Ok [ user ] ->
      let h = Digestif.SHA256.digest_string c.password in
      Digestif.SHA256.equal h user.password
  | _ -> false

let login req server { secret; _ } =
  let open Vif.Response.Syntax in
  match Vif.Request.of_multipart_form req with
  | Ok (c : credentials) when login server c ->
    let token = Jwto.encode HS512 secret [ "username", c.username ] in
    let token = Result.get_ok token in
    let str = "Authenticated!\n" in
    let* () = Vif.Cookie.set ~name:"jwt" server req token in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.redirect_to req Vif.Uri.(rel /?? any)
  | Ok _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Error _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid multipart/form-data\n" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)

let subscribe req server _ =
  let pool = Vif.Server.device caqti server in
  let ( let* ) = Result.bind in
  let search =
    let open Caqti_request.Infix in
    Caqti_type.(string ->* user)
      "SELECT * FROM users WHERE username = ?" in
  let insert =
    let open Caqti_request.Infix in
    Caqti_type.(t2 string string ->. unit)
      "INSERT INTO users (username, password) VALUES (?, ?)" in
  let already_exists (c : credentials) (module Conn : Caqti_miou.CONNECTION) =
    let* user = Conn.collect_list search c.username in
    Ok (List.is_empty user == false) in
  let insert (c : credentials) (module Conn : Caqti_miou.CONNECTION) =
    let hash = Digestif.SHA256.digest_string c.password in
    let hash = Digestif.SHA256.to_hex hash in
    Conn.exec insert (c.username, hash) in
  let result =
    let* c = Vif.Request.of_multipart_form req in
    let* () =
      match Caqti_miou_unix.Pool.use (already_exists c) pool with
      | Ok true -> Error (`Already_exists c.username)
      | Ok false -> Ok ()
      | Error (#Caqti_error.t as err) -> Error err in
    match Caqti_miou_unix.Pool.use (insert c) pool with
    | Ok () -> Ok ()
    | Error (#Caqti_error.t as err) -> Error err in
  let open Vif.Response.Syntax in
  match result with
  | Ok () ->
      let* () = Vif.Response.empty in
      Vif.Response.redirect_to req Vif.Uri.(rel / "login.html" /?? any)
  | Error (`Already_exists username) ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let str = Fmt.str "%S already exists.\n" username in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `Conflict
  | Error (#Caqti_error.t as err) ->
      let open Vif.Response.Syntax in
      let str = Fmt.str "SQL error: %a" Caqti_error.pp err in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `Internal_server_error
  | Error (`Invalid_multipart_form | `Not_found _) ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let str = Fmt.str "Invalid multipart/form-data\n" in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond (`Code 422)

type chatroom =
  { make : unit -> int * string Flux.source
  ; send : string -> unit
  ; shutdown : int -> unit }

let chatroom =
  let uid = Atomic.make 0 in
  let actives = Hashtbl.create 0x100 in
  let mutex = Miou.Mutex.create () in
  let make () =
    let n = Atomic.fetch_and_add uid 1 in
    Miou.Mutex.protect mutex @@ fun () ->
    let q = Flux.Bqueue.(create with_close 0x7ff) in
    Hashtbl.replace actives n q;
    n, Flux.Source.bqueue q in
  let shutdown uid =
    Miou.Mutex.protect mutex @@ fun () ->
    Hashtbl.remove actives uid in
  let send msg =
    Miou.Mutex.protect mutex @@ fun () ->
    let fn _ q = Flux.Bqueue.put q msg in
    Hashtbl.iter fn actives in
  let finally _ =
    Miou.Mutex.protect mutex @@ fun () ->
    let fn _ q = Flux.Bqueue.close q in
    Hashtbl.iter fn actives in
  Vif.Device.v ~name:"chatroom" ~finally [] @@ fun _ ->
  { make; send; shutdown }

let chat _req _server _ = Vif.Response.websocket

let websocket ic oc server _ =
  let t = Vif.Server.device chatroom server in
  let uid, src = t.make () in
  let fn str = oc (`Msg (`Text, true), str) in
  let prm0 = Miou.async @@ fun () -> Flux.Source.each fn src in
  let prm1 = Miou.async @@ fun () ->
    let rec go () =
      match ic () with
      | None | Some (`Connection_close, _) -> oc (`Connection_close, String.empty)
      | Some _ -> go () in
    go () in
  let _ = Miou.await_first [ prm0; prm1 ] in
  t.shutdown uid

type msg = { msg : string }

let msg_form =
  let open Vif.Multipart_form in
  let fn msg = { msg } in
  record fn
  |+ field "msg" string
  |> sealr

let send req server _ =
  let open Vif.Response.Syntax in
  match Vif.Request.of_multipart_form req, Vif.Request.get jwt req with
  | Ok { msg }, Some { username }->
    let t = Vif.Server.device chatroom server in
    let str = Fmt.str "%s: %s\n" username msg in
    t.send str;
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Message sent!\n" in
    Vif.Response.respond `OK
  | Error _, _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid multipart/form-data\n" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)
  | _, None ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!" in
    Vif.Response.respond `Unauthorized

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (m login_form) (rel / "login" /?? nil) --> login
  ; get (rel /?? nil) --> index
  ; post (m login_form) (rel / "subscribe" /?? nil) --> subscribe
  ; get (rel / "chat" /?? nil) --> chat
  ; post (m msg_form) (rel / "send" /?? nil) --> send ]

let () = Miou_unix.run @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  let uri = Uri.make ~scheme:"sqlite3" ~path:"vif.db" () in
  let env = { secret= "deadbeef"; sw; uri } in
  let middlewares = Vif.Middlewares.[ jwt ] in
  let handlers = [ Vif.Handler.static ?top:None ] in
  let devices = Vif.Devices.[ caqti; chatroom ] in
  Vif.run ~devices ~handlers ~middlewares ~websocket routes env
