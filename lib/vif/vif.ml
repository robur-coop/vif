let src = Logs.Src.create "vif"

module Log = (val Logs.src_log src : Logs.LOG)
module Json = Json
module U = Vif_u

module R = struct
  include Vif_r
  open Vif_t

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

module T = Vif_t
module Stream = Stream
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
    if Vif_handler.cache req path then
      let* () = with_string req "" in
      respond `Not_modified
    else
      let mime = Option.value ~default:(mime_type path) mime in
      let src = Stream.Source.file (Fpath.to_string path) in
      let src = Stream.Stream.from src in
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
      let* () = with_stream req src in
      respond `OK
end

module Cookie = Vif_cookie
module Handler = Vif_handler

type e = Response.e = Empty
type f = Response.f = Filled
type s = Response.s = Sent

let ( let* ) = Response.bind
let return = Response.return

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
      Vif_method.t option -> (c, a) Vif_t.t -> (c, a) Vif_request.t option =
   fun meth c ->
    let none = true in
    let some = ( = ) (Vif_request0.meth req0) in
    let meth_match = Option.fold ~none ~some meth in
    Log.debug (fun m -> m "method matches? %b" meth_match);
    match c with
    | Vif_t.Any as encoding ->
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
    | Multipart_form -> assert false (* TODO *)
  in
  { Vif_r.extract }

module Multipart_form = struct
  include Vif_multipart_form

  let parse req =
    let open Multipart_form_miou in
    let hdrs = Vif_request.headers req in
    let ct =
      match Vif_headers.get hdrs "content-type" with
      | None -> Fmt.invalid_arg "Content-type field missing"
      | Some str ->
          let ct = Multipart_form.Content_type.of_string (str ^ "\r\n") in
          Result.get_ok ct
    in
    let t = Bounded_stream.create 0x100 in
    let prm0 =
      Miou.async @@ fun () ->
      let open Stream in
      Stream.into (Sink.into_bstream t) (Vif_request.stream req)
    in
    let prm1 = Miou.async @@ fun () -> of_stream_to_list t ct in
    Miou.await_exn prm0;
    match Miou.await_exn prm1 with
    | Ok (_tree, lst) ->
        let fn (_id, hdrs) =
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
          let meta =
            { name= !name; filename= !filename; size= !size; mime= !mime }
          in
          (meta, hdrs)
        in
        Ok (List.map (fun (k, v) -> (fn k, v)) lst)
    | Error (`Msg msg) ->
        Logs.err (fun m -> m "Invalid multipart/form-data: %s" msg);
        Error `Invalid_multipart_form
end

module Request = struct
  include Vif_request

  let of_multipart_form : type a.
         (Vif_t.multipart_form, a) Vif_request.t
      -> (a, [> `Invalid_multipart_form | `Not_found of string ]) result =
    function
    | { encoding= Multipart_form_encoding r; _ } as req ->
        let ( let* ) = Result.bind in
        let* raw = Multipart_form.parse req in
        begin
          try Ok (Multipart_form.get_record r raw)
          with Multipart_form.Field_not_found field ->
            Error (`Not_found field)
        end
    | { encoding= Multipart_form; _ } -> assert false
    | { encoding= Any; _ } -> assert false
end

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
      Miou.async ~orphans:daemon.orphans @@ fun () ->
      match
        Vif_response.(run req0 empty) (fn daemon.server daemon.user's_value)
      with
      | Vif_response.Sent, () -> Vif_request0.close req0
      | exception exn -> Vif_request0.report_exn req0 exn
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
    (* NOTE(dinosaure): the management of the http request must finish and above
       all **not** block. Otherwise, the entire domain is blocked. Thus, the
       management of a new request transfers the user task (which can block) to
       our "daemon" instantiated in our current domain which runs cooperatively.
    *)
    begin
      Miou.Mutex.protect daemon.mutex @@ fun () ->
      Queue.push (User's_task (req0, fn)) daemon.queue;
      Miou.Condition.signal daemon.condition
    end

type config = Vif_config.config

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
let config = Vif_config.config

let process stop cfg server user's_value fn =
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
      Httpcats.Server.with_tls ~parallel ?stop ?config ~backlog:cfg.backlog tls
        ~handler:fn cfg.sockaddr;
      Miou.cancel user's_tasks
  | Some (`H2 _), None ->
      Miou.cancel user's_tasks;
      failwith "Impossible to launch an h2 server without TLS."
  | Some (`Both (config, _) | `HTTP_1_1 config), None ->
      Httpcats.Server.clear ~parallel ?stop ~config ~handler:fn cfg.sockaddr;
      Miou.cancel user's_tasks
  | None, None ->
      Log.debug (fun m -> m "Start a non-tweaked HTTP/1.1 server");
      Httpcats.Server.clear ~parallel ?stop ~handler:fn cfg.sockaddr;
      Miou.cancel user's_tasks

let store_pid = function
  | None -> ()
  | Some v ->
      let oc = open_out (Fpath.to_string v) in
      output_string oc (string_of_int (Unix.getpid ()));
      close_out oc

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

let run ?(cfg = Vif_options.config_from_globals ()) ?(devices = Ds.[])
    ?(middlewares = Ms.[]) ?(handlers = []) routes user's_value =
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () = Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  let interactive = !Sys.interactive in
  let domains = cfg.domains in
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
  let default = default_from_handlers handlers in
  let fn0 = handler cfg ~default ~middlewares routes in
  let prm0 = Miou.async @@ fun () -> process stop cfg server user's_value fn0 in
  let tasks =
    let fn _ = handler cfg ~default ~middlewares routes in
    List.init domains fn
  in
  let tasks =
    if domains > 0 then
      Miou.parallel (process stop cfg server user's_value) tasks
    else []
  in
  Miou.await_exn prm0;
  List.iter (function Ok () -> () | Error exn -> raise exn) tasks;
  Ds.finally (Vif_d.Devices devices);
  Log.debug (fun m -> m "Vif (and devices) terminated")

let setup_config = Vif_options.setup_config
