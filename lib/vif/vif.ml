let src = Logs.Src.create "vif"

module Log = (val Logs.src_log src : Logs.LOG)
module Uri = Vif_core.Uri
module Client = Vif_client_unix
module Device = Vif_core.Device
module Server = Vif_core.Server
module Queries = Vif_core.Queries
module Type = Vif_core.Type
module Method = Vif_core.Method
module Status = Vif_core.Status
module Headers = Vif_core.Headers
module Cookie = Vif_core.Cookie
module Devices = Vif_core.Devices
module Multipart_form = Vif_core.Multipart_form

module Route = struct
  include Vif_core.Route

  type nonrec 'r t = (Httpcats.Server.flow, 'r) t

  open Vif_core.Type

  type ('fu, 'return) route =
    | Handler :
        (Httpcats.Server.flow, 'f, 'x) Vif_core.Route.req
        * ('e, 'x, 'r) Vif_core.Uri.t
        -> ('f, 'r) route

  let get t = Handler (Request (Some `GET, Null), t)
  let head t = Handler (Request (Some `HEAD, Null), t)
  let delete t = Handler (Request (Some `DELETE, Null), t)
  let post c t = Handler (Request (Some `POST, c), t)
  let put c t = Handler (Request (Some `PUT, c), t)
  let route (Handler (req, t)) f = Route (req, t, f)
  let ( --> ) = route
end

module Middleware = struct
  include Vif_core.Middleware

  type nonrec ('cfg, 'v) t = (Httpcats.Server.flow, 'cfg, 'v) t
end

module Middlewares = struct
  type 'cfg t =
    | [] : 'cfg t
    | ( :: ) :
        (Httpcats.Server.flow, 'cfg, 'a) Vif_core.Middleware.t * 'cfg t
        -> 'cfg t

  type ('cfg, 'v) fn = (Httpcats.Server.flow, 'cfg, 'v) Vif_core.Middleware.fn

  let v = Vif_core.Middleware.v

  type ('value, 'a, 'c) ctx = {
      server: Vif_core.Server.t
    ; request: Httpcats.Server.flow Vif_core.Request0.t
    ; target: string
    ; user's_value: 'value
  }

  let rec run : type v.
         v t
      -> (v, 'a, 'c) ctx
      -> Vif_core.Middleware.Hmap.t
      -> Vif_core.Middleware.Hmap.t =
   fun lst ctx env ->
    match lst with
    | [] -> env
    | Middleware (fn, key) :: r -> begin
        match fn ctx.request ctx.target ctx.server ctx.user's_value with
        | Some value -> run r ctx (Vif_core.Middleware.Hmap.add key value env)
        | None -> run r ctx env
        | exception _exn -> run r ctx env
      end
end

module Response = struct
  include Vif_core.Response

  let mime_type path =
    let default = "application/octet-stream" in
    match Conan_unix.run_with_tree Conan_light.tree (Fpath.to_string path) with
    | Ok m -> Option.value ~default (Conan.Metadata.mime m)
    | Error _ -> default
    | exception _ -> default

  let with_file ?mime ?compression:alg ?etag req path =
    if
      Sys.file_exists (Fpath.to_string path) = false
      || Sys.is_directory (Fpath.to_string path)
    then Fmt.invalid_arg "Response.with_file %a" Fpath.pp path;
    if Vif_handler_unix.cached_on_client_side ?etag req path then
      let* () = with_string req "" in
      respond `Not_modified
    else
      let mime = Option.value ~default:(mime_type path) mime in
      let src = Vif_handler_unix.file (Fpath.to_string path) in
      let* _ = Vif_core.Response.connection_close req in
      let field = "content-type" in
      let* () = add ~field mime in
      let stat = Unix.stat (Fpath.to_string path) in
      let* _ = Vif_core.Response.content_length stat.Unix.st_size in
      let none = return false in
      let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
      let field = "etag" in
      let etag =
        match etag with
        | None -> Vif_handler_unix.sha256sum path
        | Some etag -> etag
      in
      let* () = add ~field etag in
      let* () = with_source req src in
      respond `OK

  type nonrec empty = empty = Empty
  type nonrec filled = filled = Filled
  type nonrec sent = sent = Sent

  module Infix = struct
    let ( >>= ) = bind
  end

  module Syntax = struct
    let ( let* ) = bind
  end
end

module Handler = struct
  include Vif_core.Handler
  include Vif_handler_unix
end

module Request = struct
  include Vif_core.Request

  type nonrec ('c, 'a) t = (Httpcats.Server.flow, 'c, 'a) t
  type nonrec request = Httpcats.Server.flow request
end

type ic = Httpcats.Server.Websocket.ic
type oc = Httpcats.Server.Websocket.oc

type 'value daemon = {
    queue: 'value user's_function Queue.t
  ; mutex: Miou.Mutex.t
  ; orphans: unit Miou.orphans
  ; condition: Miou.Condition.t
  ; user's_value: 'value
  ; server: Vif_core.Server.t
}

and 'value user's_function =
  | User's_request :
      Httpcats.Server.flow Vif_core.Request0.t * 'value fn
      -> 'value user's_function
  | User's_websocket : 'value ws -> 'value user's_function

and 'value fn =
     Vif_core.Server.t
  -> 'value
  -> (Response.empty, Response.sent, unit) Vif_core.Response.t

and 'value ws = Vif_core.Server.t -> 'value -> unit

let to_ctx daemon req0 =
  {
    Middlewares.server= daemon.server
  ; Middlewares.request= req0
  ; Middlewares.target= Vif_core.Request0.target req0
  ; Middlewares.user's_value= daemon.user's_value
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
    Queue.clear daemon.queue; lst
  in
  let fn = function
    | User's_websocket fn ->
        Log.debug (fun m -> m "start to execute a websocket handler");
        let fn () = fn daemon.server daemon.user's_value in
        ignore (Miou.async ~orphans:daemon.orphans fn)
    | User's_request (req0, fn) ->
        let tags = Vif_core.Request0.tags req0 in
        Log.debug (fun m -> m ~tags "new user's request handler");
        let now () = Int32.of_float (Unix.gettimeofday ()) in
        let fn () =
          try
            Log.debug (fun m -> m ~tags "run user's request handler");
            let Vif_core.Response.Sent, () =
              Vif_core.Response.(run ~now req0 Empty)
                (fn daemon.server daemon.user's_value)
            in
            Log.debug (fun m -> m ~tags "user's request handler terminated");
            Vif_core.Request0.close req0
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            Log.err (fun m ->
                m ~tags "Unexpected exception from the user's handler: %s"
                  (Printexc.to_string exn));
            Log.err (fun m ->
                m ~tags "%s" (Printexc.raw_backtrace_to_string bt));
            Vif_core.Request0.report_exn req0 exn
        in
        ignore (Miou.async ~orphans:daemon.orphans fn)
  in
  List.iter fn tasks; user's_functions daemon

let to_unix_file_descr = function
  | `Tls tls -> Tls_miou_unix.file_descr tls |> Miou_unix.to_file_descr
  | `Tcp file_descr -> Miou_unix.to_file_descr file_descr

let peer socket =
  let file_descr = to_unix_file_descr socket in
  match Unix.getpeername file_descr with
  | Unix.ADDR_UNIX str -> Fmt.str "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.str "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let is_localhost socket =
  let file_descr = to_unix_file_descr socket in
  match Unix.getpeername file_descr with
  | Unix.ADDR_UNIX _ -> false
  | Unix.ADDR_INET (inet_addr, _) ->
      inet_addr = Unix.inet_addr_loopback
      || inet_addr = Unix.inet6_addr_loopback

let handler ~default ~middlewares routes daemon =
  ();
  let dispatch = Route.dispatch ~default routes in
  fun socket reqd ->
    let req0 = Vif_core.Request0.of_reqd socket ~is_localhost ~peer reqd in
    let ctx = to_ctx daemon req0 in
    let env = Middlewares.run middlewares ctx Vif_core.Middleware.Hmap.empty in
    let request = Vif_core.recognize_request ~env req0 in
    let target = Vif_core.Request0.target req0 in
    let meth = Vif_core.Request0.meth req0 in
    try
      let fn = dispatch ~meth ~request ~target in
      (* NOTE(dinosaure): the management of the http request must finish and above
         all **not** block. Otherwise, the entire domain is blocked. Thus, the
         management of a new request transfers the user task (which can block) to
         our "daemon" instantiated in our current domain which runs cooperatively.
      *)
      begin
        Miou.Mutex.protect daemon.mutex @@ fun () ->
        Queue.push (User's_request (req0, fn)) daemon.queue;
        Miou.Condition.signal daemon.condition
      end
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Log.err (fun m ->
          m "Unexpected exception from dispatch: %s" (Printexc.to_string exn));
      Log.err (fun m -> m "%s" (Printexc.raw_backtrace_to_string bt));
      raise exn

let ws_handler daemon fn ?stop flow =
  let fn ic oc =
    begin
      Miou.Mutex.protect daemon.mutex @@ fun () ->
      Queue.push (User's_websocket (fn ic oc)) daemon.queue;
      Miou.Condition.signal daemon.condition
    end
  in
  Log.debug (fun m -> m "Start to upgrade a connection to websocket");
  Httpcats.Server.Websocket.upgrade ?stop ~fn flow

type config = Vif_config_unix.config

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let config = Vif_config_unix.config

let process stop cfg server user's_value ready (fn, ws_fn) =
  Logs.debug (fun m ->
      m "new HTTP server on [%d]" (Stdlib.Domain.self () :> int));
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
  let fn = fn daemon in
  let user's_tasks = Miou.async @@ fun () -> user's_functions daemon in
  let parallel = false in
  (* NOTE(dinosaure): The user task **must** be executed cooperatively (instead
     of in parallel) with the task managing the new http connection. [httpcats]
     is therefore instructed to launch the task managing the http connection on
     the same domain as the [process] domain. *)
  match (cfg.Vif_config_unix.http, cfg.Vif_config_unix.tls) with
  | config, Some tls ->
      let upgrade flow = ws_handler daemon ws_fn (`Tls flow) in
      Httpcats.Server.with_tls ~parallel ~upgrade ?stop ?config
        ~backlog:cfg.backlog tls ~ready ~handler:fn cfg.sockaddr;
      Miou.cancel user's_tasks
  | Some (`H2 _), None ->
      Miou.cancel user's_tasks;
      assert (Miou.Computation.try_return ready ());
      failwith "Impossible to launch an h2 server without TLS."
  | Some (`Both (config, _) | `HTTP_1_1 config), None ->
      let upgrade flow = ws_handler daemon ws_fn (`Tcp flow) in
      Httpcats.Server.clear ~parallel ~upgrade ?stop ~config ~ready ~handler:fn
        cfg.sockaddr;
      Miou.cancel user's_tasks
  | None, None ->
      let upgrade flow = ws_handler daemon ws_fn (`Tcp flow) in
      Log.debug (fun m -> m "Start a non-tweaked HTTP/1.1 server");
      Httpcats.Server.clear ~parallel ~upgrade ?stop ~ready ~handler:fn
        cfg.sockaddr;
      Miou.cancel user's_tasks

let store_pid = function
  | None -> ()
  | Some v ->
      Log.debug (fun m -> m "Create PID file");
      let oc = open_out (Fpath.to_string v) in
      output_string oc (string_of_int (Unix.getpid ()));
      close_out oc;
      let delete () = try Unix.unlink (Fpath.to_string v) with _exn -> () in
      at_exit delete

let default req target _server _user's_value =
  let pp_field ppf (k, v) =
    let v = String.split_on_char ' ' v in
    let v = List.map (String.split_on_char '\t') v in
    let v = List.flatten v in
    let v = List.filter_map (function "" -> None | v -> Some v) v in
    Fmt.pf ppf "%s: @[<hov>%a@]%!" k Fmt.(list ~sep:(any "@ ") string) v
  in
  let str =
    Fmt.str "Unspecified destination %s (%a):\n%a\n" target Vif_core.Method.pp
      (Vif_core.Request.meth req)
      Fmt.(list ~sep:(any "\n") pp_field)
      (Vif_core.Request.headers req)
  in
  let len = String.length str in
  let field = "content-type" in
  let open Response.Syntax in
  let* () = Vif_core.Response.add ~field "text/plain; charset=utf-8" in
  let* _ = Vif_core.Response.content_length len in
  let* () = Vif_core.Response.with_string req str in
  Vif_core.Response.respond `Not_found

let default_from_handlers handlers req target server user's_value =
  let fn acc handler =
    match acc with
    | Some _ as acc -> acc
    | None -> handler req target server user's_value
  in
  match List.fold_left fn None handlers with
  | Some p -> p
  | None -> default req target server user's_value

let rng ~with_rng =
  let finally = function
    | Some rng -> Mirage_crypto_rng_miou_unix.kill rng
    | None -> ()
  in
  Device.v ~name:"mirage-crypto" ~finally Device.[] @@ fun _ ->
  if with_rng then
    let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
    Some rng
  else None

let run ?(cfg = Vif_options_unix.config_from_globals ()) ?(devices = Devices.[])
    ?(middlewares = Middlewares.[]) ?(handlers = []) ?websocket routes
    user's_value =
  Option.iter Logs.set_reporter cfg.reporter;
  Option.iter Logs.set_level cfg.level;
  let interactive = !Sys.interactive in
  let domains = Int.min (Miou.Domain.available ()) cfg.domains in
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
  let rng = rng ~with_rng:cfg.Vif_config_unix.with_rng in
  let devices = Devices.(rng :: devices) in
  let devices = Devices.run Vif_core.Device.Hmap.empty devices user's_value in
  Logs.debug (fun m -> m "devices launched");
  let server =
    { Vif_core.Server.devices; cookie_key= cfg.Vif_config_unix.cookie_key }
  in
  let default = default_from_handlers handlers in
  let websocket =
    match websocket with
    | None -> fun _ oc _ _ -> oc (`Connection_close, String.empty)
    | Some websocket -> websocket
  in
  let fn0 = handler ~default ~middlewares routes in
  let ws_fn0 = websocket in
  let rd0 = Miou.Computation.create () in
  let prm0 =
    Miou.async @@ fun () ->
    process stop cfg server user's_value rd0 (fn0, ws_fn0)
  in
  let tasks =
    let fn _ =
      let ready = Miou.Computation.create () in
      let fn = handler ~default ~middlewares routes in
      let ws_fn = websocket in
      (ready, fn, ws_fn)
    in
    List.init domains fn
  in
  let prm1 =
    Miou.async @@ fun () ->
    let rdn = rd0 :: List.map (fun (x, _, _) -> x) tasks in
    List.iter Miou.Computation.await_exn rdn;
    store_pid cfg.pid
  in
  let prmn =
    let fn (ready, fn, ws_fn) =
      process stop cfg server user's_value ready (fn, ws_fn)
    in
    if domains > 0 then Miou.parallel fn tasks else []
  in
  Miou.await_exn prm0;
  Miou.await_exn prm1;
  List.iter (function Ok () -> () | Error exn -> raise exn) prmn;
  Devices.finally (Vif_core.Device.Devices devices);
  Log.debug (fun m -> m "Vif (and devices) terminated")

let setup_config = Vif_options_unix.setup_config
let reporter ~sources ~ppf = Vif_options_unix.reporter sources ppf
