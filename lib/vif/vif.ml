let src = Logs.Src.create "vif"

module Log = (val Logs.src_log src : Logs.LOG)
module Json = Json
module U = Vif_u

module R = struct
  include Vif_r
  open Vif_content_type

  type ('fu, 'return) t =
    | Handler : ('f, 'x) Vif_r.req * ('x, 'r) Vif_u.t -> ('f, 'r) t

  let get t = Handler (Request (Some `GET, Null), t)
  let post c t = Handler (Request (Some `POST, c), t)
  let route (Handler (req, t)) f = Route (req, t, f)
  let ( --> ) = route
end

module C = Vif_c
module D = Vif_d
module S = Vif_s
module M = Vif_m

module Ds = struct
  type 'value t =
    | [] : 'value t
    | ( :: ) : ('value, 'a) D.device * 'value t -> 'value t

  let run : Vif_d.t -> 'value t -> 'value -> Vif_d.t =
   fun t lst user's_value ->
    let rec go t = function
      | [] -> t
      | x :: r -> go (Vif_d.run t user's_value x) r
    in
    go t lst

  let finally : Vif_d.t -> unit =
   fun t ->
    let[@warning "-8"] (Vif_d.Devices m) = t in
    let fn (Vif_d.Hmap.B (k, v)) =
      let { Vif_d.Device.finally; _ } = Vif_d.Hmap.Key.info k in
      finally v
    in
    Vif_d.Hmap.iter fn m
end

module Ms = struct
  type 'cfg t = [] : 'cfg t | ( :: ) : ('cfg, 'a) Vif_m.t * 'cfg t -> 'cfg t
  type ('cfg, 'v) fn = ('cfg, 'v) Vif_m.fn

  let make = Vif_m.make

  type ('value, 'a, 'c) ctx = {
      server: Vif_s.t
    ; request: Vif_request0.t
    ; target: string
    ; user's_value: 'value
  }

  let rec run : type v. v t -> (v, 'a, 'c) ctx -> Vif_m.Hmap.t -> Vif_m.Hmap.t =
   fun lst ctx env ->
    match lst with
    | [] -> env
    | Middleware (fn, key) :: r -> begin
        match fn ctx.request ctx.target ctx.server ctx.user's_value with
        | Some value -> run r ctx (Vif_m.Hmap.add key value env)
        | None -> run r ctx env
        | exception _exn -> run r ctx env
      end
end

module Content_type = Vif_content_type
module Stream = Stream
module Method = Vif_method
module Status = Vif_status
module Headers = Vif_headers
module Request = Vif_request
module Response = Vif_response

let is_application_json { Multipart_form.Content_type.ty; subty; _ } =
  match (ty, subty) with `Application, `Iana_token "json" -> true | _ -> false

let content_type server =
  let headers =
    match server.S.reqd with
    | `V1 reqd ->
        let request = H1.Reqd.request reqd in
        H1.Headers.to_list request.H1.Request.headers
    | `V2 reqd ->
        let request = H2.Reqd.request reqd in
        H2.Headers.to_list request.H2.Request.headers
  in
  let c = List.assoc_opt "content-type" headers in
  let c = Option.map (fun c -> c ^ "\r\n") c in
  let c = Option.to_result ~none:`Not_found c in
  Result.bind c Multipart_form.Content_type.of_string

let method_of_request server =
  match server.S.reqd with
  | `V1 reqd -> ((H1.Reqd.request reqd).H1.Request.meth :> H2.Method.t)
  | `V2 reqd -> ((H2.Reqd.request reqd).H2.Request.meth :> H2.Method.t)

let request ~env server =
  let extract : type c a.
         Vif_method.t option
      -> (c, a) Vif_content_type.t
      -> (c, a) Vif_request.t option =
   fun meth c ->
    let meth' = method_of_request server in
    match (meth, meth', c) with
    | None, _, (Vif_content_type.Any as encoding) ->
        Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
    | Some a, b, (Vif_content_type.Any as encoding) ->
        if a = b then Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
        else None
    | None, _, (Null as encoding) ->
        Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
    | Some a, b, (Null as encoding) ->
        if a = b then Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
        else None
    | None, _, (Json_encoding _ as encoding) ->
        let c = content_type server in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json then
          Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
        else None
    | Some a, b, (Json_encoding _ as encoding) ->
        let c = content_type server in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json && a = b then
          Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
        else None
    | None, _, (Json as encoding) ->
        let c = content_type server in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json then
          Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
        else None
    | Some a, b, (Json as encoding) ->
        let c = content_type server in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json && a = b then
          Some (Vif_request.of_reqd ~encoding ~env server.S.reqd)
        else None
  in
  { Vif_r.extract }

let handler ~default ~middlewares routes devices user's_value =
  ();
  fun socket reqd ->
    let target =
      match reqd with
      | `V1 reqd -> (H1.Reqd.request reqd).H1.Request.target
      | `V2 reqd -> (H2.Reqd.request reqd).H2.Request.target
    in
    let server = { Vif_s.reqd; socket; devices } in
    let ctx =
      { Ms.server; request= Vif_request0.of_reqd reqd; target; user's_value }
    in
    let env = Ms.run middlewares ctx Vif_m.Hmap.empty in
    let request = request ~env server in
    Log.debug (fun m -> m "Handle a new request to %s" target);
    let fn = R.dispatch ~default routes ~request ~target in
    match fn server user's_value with Vif_response.Response -> ()

type config = Vif_config.config

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let config = Vif_config.config

let handle stop cfg fn =
  Logs.debug (fun m ->
      m "new HTTP server on [%d]" (Stdlib.Domain.self () :> int));
  match (cfg.Vif_config.http, cfg.Vif_config.tls) with
  | config, Some tls ->
      Httpcats.Server.with_tls ?stop ?config ~backlog:cfg.backlog tls
        ~handler:fn cfg.sockaddr
  | Some (`H2 _), None ->
      failwith "Impossible to launch an h2 server without TLS."
  | Some (`Both (config, _) | `HTTP_1_1 config), None ->
      Httpcats.Server.clear ?stop ~config ~handler:fn cfg.sockaddr
  | None, None ->
      Log.debug (fun m -> m "Start a non-tweaked HTTP/1.1 server");
      Httpcats.Server.clear ?stop ~handler:fn cfg.sockaddr

let store_pid = function
  | None -> ()
  | Some v ->
      let oc = open_out (Fpath.to_string v) in
      output_string oc (string_of_int (Unix.getpid ()));
      close_out oc

let run ?(cfg = Vif_options.config_from_globals ()) ?(devices = Ds.[])
    ?(middlewares = Ms.[]) ~default routes user's_value =
  let interactive = !Sys.interactive in
  let domains = Miou.Domain.available () in
  store_pid cfg.pid;
  let stop =
    match interactive with
    | true ->
        let stop = Httpcats.Server.stop () in
        let fn _sigint = Httpcats.Server.switch stop in
        let behavior = Sys.Signal_handle fn in
        ignore (Miou.sys_signal Sys.sigint behavior);
        Some stop
    | false -> None
  in
  Logs.debug (fun m -> m "Vif.run, interactive:%b" interactive);
  let[@warning "-8"] (Vif_d.Devices devices) =
    Ds.run Vif_d.empty devices user's_value
  in
  Logs.debug (fun m -> m "devices launched");
  let fn0 = handler ~default ~middlewares routes devices user's_value in
  let prm = Miou.async @@ fun () -> handle stop cfg fn0 in
  let tasks =
    List.init domains (fun _ ->
        handler ~default ~middlewares routes devices user's_value)
  in
  let tasks =
    if domains > 0 then Miou.parallel (handle stop cfg) tasks else []
  in
  Miou.await_exn prm;
  List.iter (function Ok () -> () | Error exn -> raise exn) tasks;
  Ds.finally (Vif_d.Devices devices);
  Log.debug (fun m -> m "Vif (and devices) terminated")

let setup_config = Vif_options.setup_config
