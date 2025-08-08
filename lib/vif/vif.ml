let src = Logs.Src.create "vif"

module Log = (val Logs.src_log src : Logs.LOG)
module Json = Json

module Uri = struct
  include Vif_uri

  let int =
    let prj = int_of_string and inj = string_of_int in
    Tyre.(conv prj inj (regex Vif_route.Ext.arbitrary_int))

  let string c = Tyre.regex (Vif_route.Ext.string c)
  let path = Tyre.regex Re.(rep1 any)

  let bool =
    let prj = function "true" -> true | _ -> false
    and inj x = if x then "true" else "false" in
    Tyre.(conv prj inj (regex Vif_route.Ext.bool))

  let float =
    let prj = float_of_string and inj = string_of_float in
    Tyre.(conv prj inj (regex Vif_route.Ext.float))

  let option = Tyre.opt
  let conv = Tyre.conv
end

module Route = struct
  include Vif_route
  open Vif_type

  type ('fu, 'return) route =
    | Handler : ('f, 'x) Vif_route.req * ('x, 'r) Vif_uri.t -> ('f, 'r) route

  let get t = Handler (Request (Some `GET, Null), t)
  let head t = Handler (Request (Some `HEAD, Null), t)
  let delete t = Handler (Request (Some `DELETE, Null), t)
  let post c t = Handler (Request (Some `POST, c), t)
  let put c t = Handler (Request (Some `PUT, c), t)
  let route (Handler (req, t)) f = Route (req, t, f)
  let ( --> ) = route
end

module Client = Vif_client
module Device = Vif_device
module Server = Vif_server
module Middleware = Vif_middleware
module Queries = Vif_queries

module Devices = struct
  type 'value t =
    | [] : 'value t
    | ( :: ) : ('value, 'a) Device.device * 'value t -> 'value t

  let run : Vif_device.Hmap.t -> 'value t -> 'value -> Vif_device.Hmap.t =
   fun t lst user's_value ->
    let rec go t = function
      | [] -> t
      | x :: r -> go (Vif_device.run t user's_value x) r
    in
    go t lst

  let finally : Vif_device.t -> unit =
   fun t ->
    let[@warning "-8"] (Vif_device.Devices m) = t in
    let fn (Vif_device.Hmap.B (k, v)) =
      let { Vif_device.Device.finally; _ } = Vif_device.Hmap.Key.info k in
      finally v
    in
    Vif_device.Hmap.iter fn m
end

module Middlewares = struct
  type 'cfg t =
    | [] : 'cfg t
    | ( :: ) : ('cfg, 'a) Vif_middleware.t * 'cfg t -> 'cfg t

  type ('cfg, 'v) fn = ('cfg, 'v) Vif_middleware.fn

  let v = Vif_middleware.v

  type ('value, 'a, 'c) ctx = {
      server: Vif_server.t
    ; request: Vif_request0.t
    ; target: string
    ; user's_value: 'value
  }

  let rec run : type v.
      v t -> (v, 'a, 'c) ctx -> Vif_middleware.Hmap.t -> Vif_middleware.Hmap.t =
   fun lst ctx env ->
    match lst with
    | [] -> env
    | Middleware (fn, key) :: r -> begin
        match fn ctx.request ctx.target ctx.server ctx.user's_value with
        | Some value -> run r ctx (Vif_middleware.Hmap.add key value env)
        | None -> run r ctx env
        | exception _exn -> run r ctx env
      end
end

module Type = Vif_type
module Stream = Vif_stream
module Method = Vif_method
module Status = Vif_status
module Headers = Vif_headers

module Response = struct
  include Vif_response

  let mime_type path =
    let default = "application/octet-stream" in
    match Conan_unix.run_with_tree Conan_light.tree (Fpath.to_string path) with
    | Ok m -> Option.value ~default (Conan.Metadata.mime m)
    | Error _ -> default
    | exception _ -> default

  let with_file ?mime ?compression:alg req path =
    if
      Sys.file_exists (Fpath.to_string path) = false
      || Sys.is_directory (Fpath.to_string path)
    then Fmt.invalid_arg "Response.with_file %a" Fpath.pp path;
    if Vif_handler.cached_on_client_side req path then
      let* () = with_string req "" in
      respond `Not_modified
    else
      let mime = Option.value ~default:(mime_type path) mime in
      let src = Vif_stream.Source.file (Fpath.to_string path) in
      let field = "connection" in
      let* () =
        if Vif_request.version req = 1 then add ~field "close" else return ()
      in
      let field = "content-type" in
      let* () = add ~field mime in
      let stat = Unix.stat (Fpath.to_string path) in
      let field = "content-length" in
      let* () = add ~field (string_of_int stat.Unix.st_size) in
      let none = return false in
      let* _ = Option.fold ~none ~some:(fun alg -> compression alg req) alg in
      let field = "etag" in
      let* () = add ~field (Vif_handler.sha256sum path) in
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

module Cookie = Vif_cookie
module Handler = Vif_handler

let is_application_json { Multipart_form.Content_type.ty; subty; _ } =
  match (ty, subty) with `Application, `Iana_token "json" -> true | _ -> false

let is_multipart_form_data { Multipart_form.Content_type.ty; subty; _ } =
  match (ty, subty) with
  | `Multipart, `Iana_token "form-data" -> true
  | _ -> false

let content_type req0 =
  let headers = Vif_request0.headers req0 in
  let c = Vif_headers.get headers "content-type" in
  let c = Option.map (fun c -> c ^ "\r\n") c in
  let c = Option.to_result ~none:`Not_found c in
  Result.bind c Multipart_form.Content_type.of_string

let recognize_request ~env req0 =
  let extract : type c a.
      Vif_method.t option -> (c, a) Vif_type.t -> (c, a) Vif_request.t option =
   fun meth c ->
    let none = true in
    let some = ( = ) (Vif_request0.meth req0) in
    let meth_match = Option.fold ~none ~some meth in
    Log.debug (fun m -> m "method matches? %b" meth_match);
    match c with
    | Vif_type.Any as encoding ->
        if meth_match then Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | Null as encoding ->
        if meth_match then Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | Json_encoding _ as encoding ->
        let c = content_type req0 in
        let type_match = Result.map is_application_json c in
        let type_match = Result.value ~default:false type_match in
        if type_match && meth_match then
          Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | Multipart_form_encoding _ as encoding ->
        let c = content_type req0 in
        let type_match = Result.map is_multipart_form_data c in
        let type_match = Result.value ~default:false type_match in
        if type_match && meth_match then
          Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | Json as encoding ->
        let c = content_type req0 in
        let type_match = Result.map is_application_json c in
        let type_match = Result.value ~default:false type_match in
        if type_match && meth_match then
          Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
    | Multipart_form as encoding ->
        let c = content_type req0 in
        let type_match = Result.map is_multipart_form_data c in
        let type_match = Result.value ~default:false type_match in
        if type_match && meth_match then
          Some (Vif_request.of_req0 ~encoding ~env req0)
        else None
  in
  { Vif_route.extract }

module Multipart_form = struct
  open Vif_stream

  type 'id multipart_form_context = { queue: event Queue.t; parse: int parse }
  and event = [ `Id of Multipart_form.Header.t * string Bqueue.t ]

  and 'id parse =
       [ `Eof | `String of string ]
    -> [ `Continue
       | `Done of string Bqueue.t Multipart_form.t
       | `Fail of string ]

  let rec until_await ({ queue; parse } as ctx) push acc str =
    Logs.debug (fun m -> m "until_await");
    match Queue.pop queue with
    | `Id (header, bqueue) ->
        let src = Source.of_bqueue bqueue in
        let acc = push acc (header, src) in
        until_await ctx push acc str
    | exception Queue.Empty -> begin
        match parse (`String str) with
        | `Continue -> `Continue (ctx, acc)
        | `Done _tree -> `Stop acc
        | `Fail _ -> Fmt.failwith "Invalid multipart-form/data"
      end

  let rec until_done ({ queue; parse } as ctx) push acc =
    Logs.debug (fun m -> m "until_done");
    match Queue.pop queue with
    | `Id (header, bqueue) ->
        let src = Source.of_bqueue bqueue in
        let acc = push acc (header, src) in
        until_done ctx push acc
    | exception Queue.Empty -> begin
        match parse `Eof with
        | `Continue -> until_done ctx push acc
        | `Done _tree -> acc
        | `Fail _ -> Fmt.failwith "Invalid multipart-form/data"
      end

  let multipart_form req :
      (string, Multipart_form.Header.t * string source) flow =
    let hdrs = Vif_request.headers req in
    let content_type =
      match Vif_headers.get hdrs "content-type" with
      | None -> Fmt.invalid_arg "Content-type field missing"
      | Some str ->
          Multipart_form.Content_type.of_string (str ^ "\r\n") |> Result.get_ok
    in
    let flow (Sink k) =
      let queue = Queue.create () in
      let emitters header =
        let bqueue = Bqueue.create 0x100 in
        Queue.push (`Id (header, bqueue)) queue;
        let emitter = function
          | None -> Bqueue.close bqueue
          | Some str -> Bqueue.put bqueue str
        in
        (emitter, bqueue)
      in
      let init () =
        let parse = Multipart_form.parse ~emitters content_type in
        let acc = k.init () in
        `Continue ({ queue; parse }, acc)
      in
      let push state str =
        match state with
        | `Continue (ctx, acc) -> until_await ctx k.push acc str
        | `Stop _ as state -> state
      in
      let full = function `Continue _ -> false | `Stop _ -> true in
      let stop = function
        | `Continue (ctx, acc) -> k.stop (until_done ctx k.push acc)
        | `Stop acc -> k.stop acc
      in
      Sink { init; stop; full; push }
    in
    { flow }

  let flat_parts : ('a * string source, 'a * string) flow =
    let flow (Sink k) =
      let init () = k.init () in
      let push acc (hdrs, from) =
        let str, src = Stream.run ~from ~via:Flow.identity ~into:Sink.string in
        Option.iter Source.dispose src;
        k.push acc (hdrs, str)
      in
      let full acc = k.full acc in
      let stop acc = k.stop acc in
      Sink { init; stop; full; push }
    in
    { flow }

  include Vif_multipart_form

  type part = meta = {
      name: string option
    ; filename: string option
    ; size: int option
    ; mime: string option
  }

  let mime { mime; _ } = mime
  let filename { filename; _ } = filename
  let name { name; _ } = name
  let size { size; _ } = size

  let aggregate hdrs =
    let hdrs = Multipart_form.Header.to_list hdrs in
    let name = ref None in
    let filename = ref None in
    let size = ref None in
    let mime = ref None in
    let fn = function
      | Multipart_form.Field.Field (_, Content_type, { ty; subty; _ }) ->
          let open Multipart_form.Content_type in
          let value = Fmt.str "%a/%a" Type.pp ty Subtype.pp subty in
          mime := Some value;
          None
      | Field (_, Content_encoding, _) -> None
      | Field (_, Content_disposition, t) ->
          let open Multipart_form in
          name := Content_disposition.name t;
          filename := Content_disposition.filename t;
          size := Content_disposition.size t;
          None
      | Field (fn, Field, unstrctrd) ->
          let k = (fn :> string) in
          let v = Unstrctrd.fold_fws unstrctrd in
          let v = Unstrctrd.to_utf_8_string v in
          Some (k, v)
    in
    let hdrs = List.filter_map fn hdrs in
    let meta = { name= !name; filename= !filename; size= !size; mime= !mime } in
    (hdrs, meta)

  let parse req =
    let from = Vif_request.source req in
    try
      let lst, src =
        Stream.run ~from
          ~via:Flow.(multipart_form req << flat_parts)
          ~into:Sink.list
      in
      Option.iter Source.dispose src;
      let fn (hdrs, str) =
        let hdrs, meta = aggregate hdrs in
        ((meta, hdrs), str)
      in
      Ok (List.map fn lst)
    with _exn -> Error `Invalid_multipart_form

  let stream req =
    let fn (hdrs, src) =
      let _hdrs, meta = aggregate hdrs in
      (meta, src)
    in
    Stream.from (Vif_request.source req)
    |> Stream.via (multipart_form req)
    |> Stream.map fn
end

module Request = struct
  include Vif_request

  let of_multipart_form : type a.
         (Vif_type.multipart_form, a) Vif_request.t
      -> (a, [> `Invalid_multipart_form | `Not_found of string ]) result =
    function
    | { encoding= Multipart_form_encoding r; _ } as req ->
        let ( let* ) = Result.bind in
        let* raw = Multipart_form.parse req in
        begin
          try Ok (Multipart_form.get_record r raw) with
          | Multipart_form.Field_not_found field -> Error (`Not_found field)
          | exn ->
              Logs.err (fun m ->
                  m "Unexpected exception from multipart-form/data: %s"
                    (Printexc.to_string exn));
              Error `Invalid_multipart_form
        end
    | { encoding= Multipart_form; _ } as req -> Ok (Multipart_form.stream req)
    | { encoding= Any; _ } -> assert false
end

type ic = Httpcats.Server.Websocket.ic
type oc = Httpcats.Server.Websocket.oc

type 'value daemon = {
    queue: 'value user's_function Queue.t
  ; mutex: Miou.Mutex.t
  ; orphans: unit Miou.orphans
  ; condition: Miou.Condition.t
  ; user's_value: 'value
  ; server: Vif_server.t
}

and 'value user's_function =
  | User's_request : Vif_request0.t * 'value fn -> 'value user's_function
  | User's_websocket : 'value ws -> 'value user's_function

and 'value fn =
  Vif_server.t -> 'value -> (Response.empty, Response.sent, unit) Vif_response.t

and 'value ws = Vif_server.t -> 'value -> unit

let to_ctx daemon req0 =
  {
    Middlewares.server= daemon.server
  ; Middlewares.request= req0
  ; Middlewares.target= Vif_request0.target req0
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
        let src = Vif_request0.src req0 in
        Logs.debug ~src (fun m -> m "new user's request handler");
        let fn () =
          try
            Logs.debug ~src (fun m -> m "run user's request handler");
            let Vif_response.Sent, () =
              Vif_response.(run req0 Empty)
                (fn daemon.server daemon.user's_value)
            in
            Logs.debug ~src (fun m -> m "user's request handler terminated");
            Vif_request0.close req0
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            Logs.err ~src (fun m ->
                m "Unexpected exception from the user's handler: %s"
                  (Printexc.to_string exn));
            Logs.err ~src (fun m ->
                m "%s" (Printexc.raw_backtrace_to_string bt));
            Vif_request0.report_exn req0 exn
        in
        ignore (Miou.async ~orphans:daemon.orphans fn)
  in
  List.iter fn tasks; user's_functions daemon

let handler ~default ~middlewares routes daemon =
  ();
  let dispatch = Route.dispatch ~default routes in
  fun socket reqd ->
    let req0 = Vif_request0.of_reqd socket reqd in
    let ctx = to_ctx daemon req0 in
    let env = Middlewares.run middlewares ctx Vif_middleware.Hmap.empty in
    let request = recognize_request ~env req0 in
    let target = Vif_request0.target req0 in
    try
      let fn = dispatch ~request ~target in
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

type config = Vif_config.config

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let config = Vif_config.config

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
  match (cfg.Vif_config.http, cfg.Vif_config.tls) with
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
    Fmt.str "Unspecified destination %s (%a):\n%a\n" target Vif_method.pp
      (Vif_request.meth req)
      Fmt.(list ~sep:(any "\n") pp_field)
      (Vif_request.headers req)
  in
  let len = String.length str in
  let field = "content-type" in
  let open Response.Syntax in
  let* () = Vif_response.add ~field "text/plain; charset=utf-8" in
  let field = "content-length" in
  let* () = Vif_response.add ~field (string_of_int len) in
  let* () = Vif_response.with_string req str in
  Vif_response.respond `Not_found

let default_from_handlers handlers req target server user's_value =
  let fn acc handler =
    match acc with
    | Some _ as acc -> acc
    | None -> handler req target server user's_value
  in
  match List.fold_left fn None handlers with
  | Some p -> p
  | None -> default req target server user's_value

let run ?(cfg = Vif_options.config_from_globals ()) ?(devices = Devices.[])
    ?(middlewares = Middlewares.[]) ?(handlers = []) ?websocket routes
    user's_value =
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () = Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
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
  let devices = Devices.run Vif_device.Hmap.empty devices user's_value in
  Logs.debug (fun m -> m "devices launched");
  let server = { Vif_server.devices; cookie_key= cfg.Vif_config.cookie_key } in
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
  Devices.finally (Vif_device.Devices devices);
  Log.debug (fun m -> m "Vif (and devices) terminated")

let setup_config = Vif_options.setup_config
