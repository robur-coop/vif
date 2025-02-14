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

  let run : Vif_d.Hmap.t -> 'value t -> 'value -> Vif_d.Hmap.t =
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
module Cookie = Vif_cookie

type e = Response.e = Empty
type f = Response.f = Filled
type s = Response.s = Sent

let ( let* ) = Response.bind
let return = Response.return

let is_application_json { Multipart_form.Content_type.ty; subty; _ } =
  match (ty, subty) with `Application, `Iana_token "json" -> true | _ -> false

let content_type req0 =
  let headers = Vif_request0.headers req0 in
  let c = Vif_headers.get headers "content-type" in
  let c = Option.map (fun c -> c ^ "\r\n") c in
  let c = Option.to_result ~none:`Not_found c in
  Result.bind c Multipart_form.Content_type.of_string

let recognize_request ~env req0 =
  let extract : type c a.
         Vif_method.t option
      -> (c, a) Vif_content_type.t
      -> (c, a) Vif_request.t option =
   fun meth c ->
    let meth' = Vif_request0.meth req0 in
    match (meth, meth', c) with
    | None, _, (Vif_content_type.Any as encoding) ->
        Some (Vif_request.of_req0 ~encoding ~env req0)
    | Some a, b, (Vif_content_type.Any as encoding) ->
        if a = b then Some (Vif_request.of_req0 ~encoding ~env req0) else None
    | None, _, (Null as encoding) ->
        Some (Vif_request.of_req0 ~encoding ~env req0)
    | Some a, b, (Null as encoding) ->
        if a = b then Some (Vif_request.of_req0 ~encoding ~env req0) else None
    | None, _, (Json_encoding _ as encoding) ->
        let c = content_type req0 in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json then Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | Some a, b, (Json_encoding _ as encoding) ->
        let c = content_type req0 in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json && a = b then
          Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | None, _, (Json as encoding) ->
        let c = content_type req0 in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json then Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | Some a, b, (Json as encoding) ->
        let c = content_type req0 in
        let application_json = Result.map is_application_json c in
        let application_json = Result.value ~default:false application_json in
        if application_json && a = b then
          Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
  in
  { Vif_r.extract }

type 'value daemon = {
    queue: 'value user's_function Queue.t
  ; mutex: Miou.Mutex.t
  ; orphans: unit Miou.orphans
  ; condition: Miou.Condition.t
  ; user's_value: 'value
  ; server: Vif_s.t
}

and 'value user's_function =
  | User's_task : Vif_request0.t * 'value fn -> 'value user's_function

and 'value fn = Vif_s.t -> 'value -> (e, s, unit) Vif_response.t

let to_ctx daemon req0 =
  {
    Ms.server= daemon.server
  ; Ms.request= req0
  ; Ms.target= Vif_request0.target req0
  ; Ms.user's_value= daemon.user's_value
  }

let rec clean_up orphans =
  match Miou.care orphans with
  | None -> ()
  | Some None -> ()
  | Some (Some prm) -> begin
      match Miou.await prm with
      | Ok () -> clean_up orphans
      | Error exn ->
          let bt = Printexc.get_raw_backtrace () in
          Log.err (fun m -> m "User's exception: %s" (Printexc.to_string exn));
          Log.err (fun m -> m "%s" (Printexc.raw_backtrace_to_string bt));
          clean_up orphans
    end

let rec user's_functions daemon =
  clean_up daemon.orphans;
  let tasks =
    Miou.Mutex.protect daemon.mutex @@ fun () ->
    while Queue.is_empty daemon.queue do
      Miou.Condition.wait daemon.condition daemon.mutex
    done;
    let lst = List.of_seq (Queue.to_seq daemon.queue) in
    Queue.drop daemon.queue; lst
  in
  let fn (User's_task (req0, fn)) =
    let _prm =
      Miou.call ~orphans:daemon.orphans @@ fun () ->
      let response = fn daemon.server daemon.user's_value in
      match Vif_response.(run req0 empty) response with
      | Vif_response.Sent, () -> Vif_request0.close req0
    in
    ()
  in
  List.iter fn tasks; user's_functions daemon

let handler _cfg ~default ~middlewares routes daemon =
  ();
  fun socket reqd ->
    let req0 = Vif_request0.of_reqd socket reqd in
    let ctx = to_ctx daemon req0 in
    let env = Ms.run middlewares ctx Vif_m.Hmap.empty in
    let request = recognize_request ~env req0 in
    let target = Vif_request0.target req0 in
    let fn = R.dispatch ~default routes ~request ~target in
    begin
      Miou.Mutex.protect daemon.mutex @@ fun () ->
      Queue.push (User's_task (req0, fn)) daemon.queue;
      Miou.Condition.signal daemon.condition
    end

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
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () = Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  let interactive = !Sys.interactive in
  let domains = Miou.Domain.available () in
  store_pid cfg.pid;
  let stop =
    match interactive with
    | true ->
        let stop = Httpcats.Server.stop () in
        let fn _sigint =
          Log.debug (fun m -> m "Server shutdown request (SIGINT)");
          Httpcats.Server.switch stop
        in
        let behavior = Sys.Signal_handle fn in
        ignore (Miou.sys_signal Sys.sigint behavior);
        Some stop
    | false -> None
  in
  Logs.debug (fun m -> m "Vif.run, interactive:%b" interactive);
  let devices = Ds.run Vif_d.Hmap.empty devices user's_value in
  Logs.debug (fun m -> m "devices launched");
  let server = { Vif_s.devices; cookie_key= cfg.Vif_config.cookie_key } in
  let daemon =
    {
      queue= Queue.create ()
    ; mutex= Miou.Mutex.create ()
    ; orphans= Miou.orphans ()
    ; condition= Miou.Condition.create ()
    ; user's_value
    ; server
    }
  in
  let user's_tasks = Miou.call @@ fun () -> user's_functions daemon in
  let fn0 = handler cfg ~default ~middlewares routes daemon in
  let prm0 = Miou.async @@ fun () -> handle stop cfg fn0 in
  let tasks =
    let fn _ = handler cfg ~default ~middlewares routes daemon in
    List.init domains fn
  in
  let tasks =
    if domains > 0 then Miou.parallel (handle stop cfg) tasks else []
  in
  Miou.await_exn prm0;
  List.iter (function Ok () -> () | Error exn -> raise exn) tasks;
  Ds.finally (Vif_d.Devices devices);
  Miou.cancel user's_tasks;
  Log.debug (fun m -> m "Vif (and devices) terminated")

let setup_config = Vif_options.setup_config
