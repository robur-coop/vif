module Handler = Vif_handler
module Request0 = Vif_request0
module Headers = Vif_headers
module Response = Vif_response
module Type = Vif_type
module Device = Vif_device
module Server = Vif_server
module Middleware = Vif_middleware
module Queries = Vif_queries
module Method = Vif_method
module Status = Vif_status
module Cookie = Vif_cookie
module Route = Vif_route
module Tags = Vif_tags

let src = Logs.Src.create "vif.core"

module Log = (val Logs.src_log src : Logs.LOG)

module Uri = struct
  include Vif_uri

  let int =
    let prj = int_of_string and inj = string_of_int in
    Tyre.(conv prj inj (regex Vif_route.Ext.arbitrary_int))

  let string c = Tyre.regex (Vif_route.Ext.string c)
  let rest = Tyre.regex Re.(rep1 any)
  let path = Tyre.regex Re.(rep1 (compl [ char '?' ]))

  let bool =
    let prj = function "true" -> true | _ -> false
    and inj x = if x then "true" else "false" in
    Tyre.(conv prj inj (regex Vif_route.Ext.bool))

  let float =
    let prj = float_of_string and inj = string_of_float in
    Tyre.(conv prj inj (regex Vif_route.Ext.float))

  let option = Tyre.opt
  let conv = Tyre.conv

  let execp uri s =
    let re = Vif_route.get_re uri in
    Re.execp (Re.compile (Re.whole_string re)) s

  let extract uri s f =
    let _i'dunno, re_url, re = Vif_route.re_url 1 uri in
    let id, re = Re.mark re in
    let subs = Re.exec_opt (Re.compile (Re.whole_string re)) s in
    match subs with
    | Some subs ->
        if Re.Mark.test subs id then
          try Ok (Vif_route.extract_url ~original:s re_url subs f)
          with Vif_route.Tyre_exn exn -> Error (`Converter_failure exn)
        else Error `No_match
    | None -> Error `No_match
end

module Devices = struct
  type 'value t =
    | [] : 'value t
    | ( :: ) : ('value, 'a) Vif_device.device * 'value t -> 'value t

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
         Vif_method.t option
      -> (c, a) Vif_type.t
      -> ('s, c, a) Vif_request.t option =
   fun meth c ->
    let none = true in
    let some = ( = ) (Vif_request0.meth req0) in
    let meth_match = Option.fold ~none ~some meth in
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
  open Flux

  type 'id multipart_form_context = {
      queue: event Queue.t
    ; parse: int parse
    ; actives: string Flux.Bqueue.c list
  }

  and event = [ `Id of Multipart_form.Header.t * string Flux.Bqueue.c ]

  and 'id parse =
       [ `Eof | `String of string ]
    -> [ `Continue
       | `Done of string Flux.Bqueue.c Multipart_form.t
       | `Fail of string ]

  let rec until_await ~tags ({ queue; parse; actives } as ctx) push acc str =
    match Queue.pop queue with
    | `Id (header, bqueue) ->
        let src = Source.bqueue bqueue in
        let acc = push acc (header, src) in
        let ctx = { ctx with actives= bqueue :: actives } in
        until_await ~tags ctx push acc str
    | exception Queue.Empty -> begin
        match parse (`String str) with
        | `Continue -> `Continue (ctx, acc)
        | `Done _tree -> `Stop acc
        | `Fail msg ->
            List.iter Bqueue.close actives;
            Log.err (fun m -> m ~tags "Invalid multipart/form-data: %s" msg);
            `Stop acc
      end

  let rec until_done ~tags ({ queue; parse; actives } as ctx) push acc =
    match Queue.pop queue with
    | `Id (header, bqueue) ->
        let src = Source.bqueue bqueue in
        let acc = push acc (header, src) in
        let ctx = { ctx with actives= bqueue :: actives } in
        until_done ~tags ctx push acc
    | exception Queue.Empty -> begin
        match parse `Eof with
        | `Continue -> until_done ~tags ctx push acc
        | `Done _tree -> acc
        | `Fail msg ->
            List.iter Bqueue.close actives;
            Log.err (fun m -> m ~tags "Invalid multipart/form-data: %s" msg);
            acc
      end

  let multipart_form req :
      (string, Multipart_form.Header.t * string source) flow =
    let hdrs = Vif_request.headers req in
    let tags = Vif_request.tags req in
    let content_type =
      match Vif_headers.get hdrs "content-type" with
      | None -> Fmt.invalid_arg "Content-type field missing"
      | Some str ->
          Multipart_form.Content_type.of_string (str ^ "\r\n") |> Result.get_ok
    in
    let flow (Sink k) =
      let queue = Queue.create () in
      let emitters header =
        let bqueue = Flux.Bqueue.(create with_close 0x7ff) in
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
        `Continue ({ queue; parse; actives= [] }, acc)
      in
      let push state str =
        match state with
        | `Continue (ctx, acc) -> until_await ~tags ctx k.push acc str
        | `Stop _ as state -> state
      in
      let full = function `Continue _ -> false | `Stop _ -> true in
      let stop = function
        | `Continue (ctx, acc) -> k.stop (until_done ~tags ctx k.push acc)
        | `Stop acc -> k.stop acc
      in
      Sink { init; stop; full; push }
    in
    { flow }

  let flat_parts : ('a * string source, 'a * string Miou.t) flow =
    let flow (Sink k) =
      let init () = k.init () in
      let push acc (hdrs, from) =
        (* NOTE(dinosaure): Here, consumption must be asynchronous. The
           composition of several flows (done in the [parse] function) has no
           idea about scheduling: that is, this composition is done strictly
           sequentially (we execute [multipart_form.push], then we execute
           [flat_parts.push]). A problem arises when [flat_parts] has not
           consumed everything (when the part is larger than 1K) and therefore
           blocks. However, since the composition between flows does not
           recognise the idea of scheduling, if [flat_parts.push] blocks, it
           also blocks [multipart_form.push]. This results in a kind of
           _deadlock_.

           We therefore need to create tasks for each entry that consumes the
           content and ensure that the [push] function of [flat_tasks] never
           blocks. [multipart_form] can then continue without being blocked. *)
        let prm =
          Miou.async @@ fun () ->
          let via = Flow.identity in
          let into = Sink.string in
          let str, src = Stream.run ~from ~via ~into in
          Option.iter Source.dispose src;
          str
        in
        k.push acc (hdrs, prm)
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
      (* NOTE(dinosaure): all [prm] from [lst] are normally done since
         [multipart_form] has normally consumed everything from [from]. *)
      let fn (hdrs, prm) =
        let hdrs, meta = aggregate hdrs in
        ((meta, hdrs), Miou.await_exn prm)
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
         ('s, Vif_type.multipart_form, a) Vif_request.t
      -> (a, [> `Invalid_multipart_form | `Not_found of string ]) result =
    function
    | { encoding= Multipart_form_encoding r; _ } as req ->
        let ( let* ) = Result.bind in
        let* raw = Multipart_form.parse req in
        begin try Ok (Multipart_form.get_record r raw) with
        | Multipart_form.Field_not_found field -> Error (`Not_found field)
        | exn ->
            let tags = Vif_request.tags req in
            Log.err (fun m ->
                m ~tags "Unexpected exception from multipart-form/data: %s"
                  (Printexc.to_string exn));
            Error `Invalid_multipart_form
        end
    | { encoding= Multipart_form; _ } as req -> Ok (Multipart_form.stream req)
    | { encoding= Any; _ } -> assert false
end
