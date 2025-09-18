let src = Logs.Src.create "vif.request0"

module Log = (val Logs.src_log src : Logs.LOG)

type 'socket t = {
    request: request
  ; tls: Tls.Core.epoch_data option
  ; reqd: reqd
  ; socket: 'socket
  ; on_localhost: bool
  ; body: [ `V1 of H1.Body.Reader.t | `V2 of H2.Body.Reader.t ]
  ; queries: (string * string list) list
  ; src: Logs.src
}

and reqd = Httpcats_core.Server.reqd

(* and socket = [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ] *)
and request = V1 of H1.Request.t | V2 of H2.Request.t

let accept { request; _ } =
  let hdrs =
    match request with
    | V1 req -> H1.Headers.to_list req.H1.Request.headers
    | V2 req -> H2.Headers.to_list req.H2.Request.headers
  in
  match Vif_headers.get hdrs "accept" with
  | None -> []
  | Some str ->
      let types = String.split_on_char ',' str in
      let types = List.map String.trim types in
      let fn str =
        match String.split_on_char ';' str with
        | [] -> assert false
        | [ mime_type; p ] ->
            let p = String.trim p in
            let p =
              if String.starts_with ~prefix:"q=" p then
                try float_of_string String.(sub p 2 (length p - 2))
                with _ -> 1.0
              else 1.0
            in
            (String.trim mime_type, p)
        | mime_type :: _ -> (String.trim mime_type, 1.0)
      in
      let types = List.map fn types in
      let types = List.sort (fun (_, a) (_, b) -> Float.compare b a) types in
      List.map fst types

(*
let peer { socket; _ } =
  let file_descr =
    match socket with
    | `Tcp v -> Miou_unix.to_file_descr v
    | `Tls v -> Miou_unix.to_file_descr (Tls_miou_unix.file_descr v)
  in
  match Unix.getpeername file_descr with
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.str "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX v -> Fmt.str "<%s>" v
*)

let src { src; _ } = src

let to_source ~src ~schedule ~close body =
  Vif_stream.Source.with_task ~limit:0x100 @@ fun bqueue ->
  let rec on_eof () =
    close body;
    Vif_stream.Bqueue.close bqueue;
    Logs.debug ~src (fun m -> m "-> request body closed")
  and on_read bstr ~off ~len =
    let str = Bigstringaf.substring bstr ~off ~len in
    Logs.debug ~src (fun m -> m "-> + %d byte(s)" (String.length str));
    Vif_stream.Bqueue.put bqueue str;
    schedule body ~on_eof ~on_read
  in
  Log.debug (fun m -> m "schedule a reader");
  schedule body ~on_eof ~on_read

let to_source ~src = function
  | `V1 reqd ->
      let body = H1.Reqd.request_body reqd in
      to_source ~src ~schedule:H1.Body.Reader.schedule_read
        ~close:H1.Body.Reader.close body
  | `V2 reqd ->
      let body = H2.Reqd.request_body reqd in
      to_source ~src ~schedule:H2.Body.Reader.schedule_read
        ~close:H2.Body.Reader.close body

let of_reqd ?(with_tls = Fun.const None) ?(peer = Fun.const "<socket>")
    ?(is_localhost = Fun.const false) socket reqd =
  let request, body =
    match reqd with
    | `V1 reqd -> (V1 (H1.Reqd.request reqd), `V1 (H1.Reqd.request_body reqd))
    | `V2 reqd -> (V2 (H2.Reqd.request reqd), `V2 (H2.Reqd.request_body reqd))
  in
  let target =
    match request with
    | V1 req -> req.H1.Request.target
    | V2 req -> req.H2.Request.target
  in
  let tls = with_tls socket in
  let on_localhost, src =
    (is_localhost socket, Logs.Src.create (Fmt.str "vif:%s" (peer socket)))
    (*
    match Unix.getpeername fd with
    | Unix.ADDR_UNIX str ->
        let src = Logs.Src.create (Fmt.str "vif:<%s>" str) in
        (false, src)
    | Unix.ADDR_INET (inet_addr, port) ->
        let src =
          Logs.Src.create
            (Fmt.str "vif:%s:%d" (Unix.string_of_inet_addr inet_addr) port)
        in
        ( inet_addr = Unix.inet_addr_loopback
          || inet_addr = Unix.inet6_addr_loopback
        , src )
    *)
  in
  let queries = Pct.query_of_target target in
  { request; tls; reqd; socket; on_localhost; body; queries; src }

let headers { request; _ } =
  match request with
  | V1 req -> H1.Headers.to_list req.H1.Request.headers
  | V2 req -> H2.Headers.to_list req.H2.Request.headers

let queries { queries; _ } = queries

let meth { request; _ } =
  match request with
  | V1 req -> req.H1.Request.meth
  | V2 req -> req.H2.Request.meth

let target { request; _ } =
  match request with
  | V1 req -> req.H1.Request.target
  | V2 req -> req.H2.Request.target

let request_body { reqd; _ } =
  match reqd with
  | `V1 reqd -> `V1 (H1.Reqd.request_body reqd)
  | `V2 reqd -> `V2 (H2.Reqd.request_body reqd)

let report_exn { reqd; _ } exn =
  match reqd with
  | `V1 reqd -> H1.Reqd.report_exn reqd exn
  | `V2 reqd -> H2.Reqd.report_exn reqd exn

let version { request; _ } = match request with V1 _ -> 1 | V2 _ -> 2
let tls { tls; _ } = tls
let on_localhost { on_localhost; _ } = on_localhost
let reqd { reqd; _ } = reqd

let source { reqd; src; _ } =
  Logs.debug ~src (fun m -> m "the user request for a source of the request");
  to_source ~src reqd

let close { body; src; _ } =
  Logs.debug ~src (fun m -> m "close the reader body");
  match body with
  | `V1 body -> H1.Body.Reader.close body
  | `V2 body -> H2.Body.Reader.close body
