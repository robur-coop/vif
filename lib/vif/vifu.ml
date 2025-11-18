let src = Logs.Src.create "vifu"

module Log = (val Logs.src_log src : Logs.LOG)
module Json = Vif_core.Json
module Uri = Vif_core.Uri
module Device = Vif_core.Device
module Server = Vif_core.Server
module Queries = Vif_core.Queries
module Type = Vif_core.Type
module Stream = Vif_core.Stream
module Method = Vif_core.Method
module Status = Vif_core.Status
module Headers = Vif_core.Headers
module Cookie = Vif_core.Cookie
module Devices = Vif_core.Devices
module Multipart_form = Vif_core.Multipart_form

module Handler = struct
  include Vif_core.Handler

  type nonrec ('c, 'value) t = (Mhttp.flow, 'c, 'value) t
end

module Response = struct
  include Vif_core.Response

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

module Route = struct
  include Vif_core.Route

  type nonrec 'r t = (Mhttp.flow, 'r) t

  open Vif_core.Type

  type ('fu, 'return) route =
    | Handler :
        (Mhttp.flow, 'f, 'x) Vif_core.Route.req * ('x, 'r) Vif_core.Uri.t
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

  type nonrec ('cfg, 'v) t = (Mhttp.flow, 'cfg, 'v) t
end

module Middlewares = struct
  type 'cfg t =
    | [] : 'cfg t
    | ( :: ) : (Mhttp.flow, 'cfg, 'a) Vif_core.Middleware.t * 'cfg t -> 'cfg t

  type ('cfg, 'v) fn = (Mhttp.flow, 'cfg, 'v) Vif_core.Middleware.fn

  let v = Vif_core.Middleware.v

  type ('value, 'a, 'c) ctx = {
      server: Vif_core.Server.t
    ; request: Mhttp.flow Vif_core.Request0.t
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

module Request = struct
  include Vif_core.Request

  type nonrec ('c, 'a) t = (Mhttp.flow, 'c, 'a) t
  type nonrec request = Mhttp.flow request
end

module Config = struct
  type t = {
      http:
        [ `HTTP_1_1 of H1.Config.t
        | `H2 of H2.Config.t
        | `Both of H1.Config.t * H2.Config.t ]
        option
    ; tls: Tls.Config.server option
    ; port: int
    ; cookie_key: Mirage_crypto.AES.GCM.key
  }

  let really_bad_secret =
    let open Digestif in
    let hash = SHA256.digest_string "\xde\xad\xbe\xef" in
    let hash = SHA256.to_raw_string hash in
    Mirage_crypto.AES.GCM.of_secret hash

  let v ?(cookie_key = really_bad_secret) ?http ?tls port =
    let http =
      match http with
      | Some (`H1 cfg) -> Some (`HTTP_1_1 cfg)
      | Some (`H2 cfg) -> Some (`H2 cfg)
      | Some (`Both (h1, h2)) -> Some (`Both (h1, h2))
      | None -> None
    in
    { http; tls; cookie_key; port }
end

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
      Mhttp.flow Vif_core.Request0.t * 'value fn
      -> 'value user's_function

and 'value fn =
     Vif_core.Server.t
  -> 'value
  -> (Response.empty, Response.sent, unit) Vif_core.Response.t

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
    | User's_request (req0, fn) ->
        let src = Vif_core.Request0.src req0 in
        Logs.debug ~src (fun m -> m "new user's request handler");
        let now () = Int32.of_int (Mkernel.clock_wall ()) in
        let fn () =
          try
            Logs.debug ~src (fun m -> m "run user's request handler");
            let Vif_core.Response.Sent, () =
              Vif_core.Response.(run ~now req0 Empty)
                (fn daemon.server daemon.user's_value)
            in
            Logs.debug ~src (fun m -> m "user's request handler terminated");
            Vif_core.Request0.close req0
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            Logs.err ~src (fun m ->
                m "Unexpected exception from the user's handler: %s"
                  (Printexc.to_string exn));
            Logs.err ~src (fun m ->
                m "%s" (Printexc.raw_backtrace_to_string bt));
            Vif_core.Request0.report_exn req0 exn
        in
        ignore (Miou.async ~orphans:daemon.orphans fn)
  in
  List.iter fn tasks; user's_functions daemon

let handler ~default ~middlewares routes daemon =
  ();
  let dispatch = Route.dispatch ~default routes in
  fun socket reqd ->
    let req0 = Vif_core.Request0.of_reqd socket reqd in
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

let process cfg server tcpv4 user's_value ready fn =
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
  (* NOTE(dinosaure): The user task **must** be executed cooperatively (instead
     of in parallel) with the task managing the new http connection. [httpcats]
     is therefore instructed to launch the task managing the http connection on
     the same domain as the [process] domain. *)
  match (cfg.Config.http, cfg.Config.tls) with
  | _config, Some _tls ->
      assert false
      (*
      let upgrade flow = ws_handler daemon ws_fn (`Tls flow) in
      Httpcats.Server.with_tls ~parallel ~upgrade ?stop ?config
        ~backlog:cfg.backlog tls ~ready ~handler:fn cfg.sockaddr;
      Miou.cancel user's_tasks
      *)
  | Some (`H2 _), None ->
      Miou.cancel user's_tasks;
      assert (Miou.Computation.try_return ready ());
      failwith "Impossible to launch an h2 server without TLS."
  | Some (`Both (config, _) | `HTTP_1_1 config), None ->
      Mhttp.clear ~config ~ready ~handler:fn ~port:cfg.port tcpv4;
      Miou.cancel user's_tasks
  | None, None ->
      Log.debug (fun m -> m "Start a non-tweaked HTTP/1.1 server");
      Mhttp.clear ~ready ~handler:fn ~port:cfg.port tcpv4;
      Miou.cancel user's_tasks

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

let run ~cfg ?(devices = Devices.[]) ?(middlewares = Middlewares.[])
    ?(handlers = []) tcpv4 routes user's_value =
  let devices = Devices.run Vif_core.Device.Hmap.empty devices user's_value in
  let server = { Vif_core.Server.devices; cookie_key= cfg.Config.cookie_key } in
  let default = default_from_handlers handlers in
  let fn0 = handler ~default ~middlewares routes in
  let rd0 = Miou.Computation.create () in
  let prm0 =
    Miou.async @@ fun () -> process cfg server tcpv4 user's_value rd0 fn0
  in
  Miou.await_exn prm0
