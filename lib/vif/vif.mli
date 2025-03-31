module U : sig
  type 'a atom = 'a Tyre.t

  val int : int atom
  val string : string atom
  val option : 'a atom -> 'a option atom
  val conv : ('a -> 'b) -> ('b -> 'a) -> 'a atom -> 'b atom

  type ('f, 'r) path

  val rel : ('r, 'r) path
  val host : string -> ('r, 'r) path
  val ( / ) : ('f, 'r) path -> string -> ('f, 'r) path
  val ( /% ) : ('f, 'a -> 'r) path -> 'a atom -> ('f, 'r) path

  type ('f, 'r) query

  val nil : ('r, 'r) query
  val any : ('r, 'r) query
  val ( ** ) : string * 'a atom -> ('f, 'r) query -> ('a -> 'f, 'r) query

  type ('f, 'r) t

  val ( /? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
  val ( //? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
  val ( /?? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
  val keval : ('f, 'r) t -> (string -> 'r) -> 'f
  val eval : ('f, string) t -> 'f
end

module Json = Json
module S = Vif_s

module Headers : sig
  type t = (string * string) list

  val add_unless_exists : t -> string -> string -> t
  val get : t -> string -> string option
end

module Method : sig
  type t =
    [ `CONNECT
    | `DELETE
    | `GET
    | `HEAD
    | `OPTIONS
    | `POST
    | `PUT
    | `TRACE
    | `Other of string ]
end

module Multipart_form : sig
  (** Vif proposes a way to describe a form via types in order to decode a
      [multipart-form/data] request and obtain an OCaml record.

      Let's take a form such as:

      {[
        <form>
          <label for="username">Username:</label>
          <input type="text" name="username" id="username" required />
          <label for="password">Password:</label>
          <input type="password" name="password" id="password" required />
        </form>
      ]}

      It is possible to describe this server-side form in this way:

      {[
        type credential = { username: string; password: string }

        let form =
          let open Vif.Multipart_form in
          let fn username password = { username; password } in
          record fn
          |+ field "username" string
          |+ field "password" string
          |> sealr
      ]}

      It is then possible to define a POST route to manage such a form:

      {[
        let login req _server _cfg =
          match Vif.Request.of_multipart_form req with
          | Ok { username; password; } -> ...
          | Error _ -> ...

        let routes =
          let open Vif.U in
          let open Vif.R in
          let open Vif.R in
          [ post (m form) (rel / "login" /?? nil) --> login ]
      ]} *)

  type 'a t
  (** The type for runtime representation of forms of type ['a]. *)

  type 'a atom
  (** The type for runtime representation of fields of type ['a]. *)

  val string : string atom
  (** [string] is a representation of the string type. *)

  val int : int atom
  (** [int] is a representation of the int type. *)

  type ('a, 'b, 'c) orecord
  (** The type for representing open records of type ['a] with a constructor of
      type ['b]. ['c] represents the remaining fields to be described using the
      {!val:(|+)} operator. An open record initially satisfies ['c = 'b] and can
      be {{!val:sealr} sealed} once ['c = 'a]. *)

  val record : 'b -> ('a, 'b, 'b) orecord
  (** [record fn] is an incomplete representation of the record of type ['a]
      with constructor [fn]. To complete the representation, add fields with
      {!val:(|+)} and then seal the record with {!val:sealr}. *)

  type 'a field
  (** The type for fields belonging to a record of type ['a]. *)

  val field : string -> 'a atom -> 'a field
  (** [field name t] is the representation of the field called [name] of type
      [t]. The name must be unique in relation to the other fields in the form
      and must also correspond to the name given to the [<input />] in the form.
  *)

  val ( |+ ) : ('a, 'b, 'c -> 'd) orecord -> 'c field -> ('a, 'b, 'd) orecord
  (** [record |+ field] is the open record [record] augmented with the field
      [field]. *)

  val sealr : ('a, 'b, 'a) orecord -> 'a t
  (** [sealr record] seals the open record [record].

      @raise Invalid_argument if two or more fields share the same name. *)

  (** {3 Streaming API of [multipart-form/data] requests.}

      The user may want to manage a request containing a form in the form of a
      stream. This is particularly useful if you want to upload a file (and,
      instead of storing it in memory, directly write the received file to a
      temporary file). *)

  type part
  (** Type of a part from the [multipart-form/data] stream.

      A part can specify several items of information such as:
      - its {!val:name} (from the [name] parameter of the HTML tag)
      - whether this part comes from a user file (with the
        {{!val:filename} name} of the file)
      - the {{!val:mime} MIME type} of the content (according to the client)
      - as well as the {!val:size} of the content *)

  val name : part -> string option
  (** [name part] is the name of the part (from the [name] parameter of the HTML
      tag [<input />]). *)

  val filename : part -> string option
  (** [filename part] is the client's filename of the uploaded file. *)

  val mime : part -> string option
  (** [mime part] is the MIME type according to the client's webbrowser. *)

  val size : part -> int option
  (** [size part] is the size of the part in bytes. *)

  type stream = (part * string S.source) S.stream
  (** Type of a [multipart-form/data] stream.

      It may be necessary to save part of a form as a file rather than storing
      it in memory. Vif allows this using its form stream API. The user can
      observe the parts of a form one after the other and manipulate the content
      of these parts in the form of a stream:

      {[
        open Vif

        let upload req server _ =
          let fn (part, src) =
            match Multipart_form.name part with
            | Some "file" -> S.Stream.to_file "foo.txt" (S.Stream.from src)
            | Some "name" ->
                let value = S.(Stream.into Sink.string (Stream.from src)) in
                Hashtbl.add form "name" value
            | _ -> S.Stream.(drain (from src))
          in
          let stream = Result.get_ok (Request.of_multipart_form req) in
          S.Stream.each fn stream;
          match Hashtbl.find_opt form "name" with
          | Some value ->
              Unix.rename "foo.txt" value;
              Response.respond `OK
          | _ -> Response.respond `Bad_request
      ]}

      {b Note:} It is important to [drain] the parts that we are not familiar
      with. *)
end

module T : sig
  (** {3:type-of-requests Type of requests.}

      Vif is able to dispatch requests not only by the route but also by the
      type of content of the request (given by the ["Content-Type"]). These
      values represent a certain type such as the type "application/json" or
      "multipart-form/data". These last two can be completed by an "encoding"
      making it possible to transform the {i raw} content of the requests into
      an OCaml value. *)

  type null
  type json
  type multipart_form
  type ('c, 'a) t

  val null : (null, unit) t
  val json : (json, Json.t) t
  val json_encoding : 'a Json_encoding.encoding -> (json, 'a) t
  val m : 'a Multipart_form.t -> (multipart_form, 'a) t
  val multipart_form : (multipart_form, Multipart_form.stream) t
  val any : ('c, string) t
end

module M : sig
  type ('cfg, 'v) t
end

module Request : sig
  type ('c, 'a) t

  val target : ('c, 'a) t -> string
  val meth : ('c, 'a) t -> Method.t
  val version : ('c, 'a) t -> int
  val headers : ('c, 'a) t -> Headers.t
  val of_json : (T.json, 'a) t -> ('a, [ `Msg of string ]) result

  val of_multipart_form :
       (T.multipart_form, 'a) t
    -> ('a, [ `Not_found of string | `Invalid_multipart_form ]) result

  val source : ('c, 'a) t -> string S.source
  val get : ('cfg, 'v) M.t -> ('c, 'a) t -> 'v option

  (** {3:request-middleware Requests for middlewares}

      As soon as it comes to executing the various middlewares ({!type:M.t})
      defined by the user, the latter can manipulate the HTTP request given by
      the client. However, the latter has a limitation: the body of the request
      {b cannot} be obtained from the {!type:request} type of value. Indeed,
      middlewares should not manipulate the body of requests and should only
      refer to meta-data (such as {{!val:headers_of_request} headers}). *)

  type request

  val headers_of_request : request -> Headers.t
  val method_of_request : request -> Method.t
  val target_of_request : request -> string
end

module R : sig
  type 'r route
  type ('fu, 'return) t

  val get : ('x, 'r) U.t -> ((T.null, unit) Request.t -> 'x, 'r) t
  val post : ('c, 'a) T.t -> ('x, 'r) U.t -> (('c, 'a) Request.t -> 'x, 'r) t
  val ( --> ) : ('f, 'r) t -> 'f -> 'r route
end

module C : sig
  (** Module [C] implements the {b c}lient part of the HTTP protocol. *)

  type body
  type response

  type resolver =
    [ `Happy of Happy_eyeballs_miou_unix.t
    | `User of Httpcats.resolver
    | `System ]

  val request :
       ?config:[ `HTTP_1_1 of H1.Config.t | `H2 of H2.Config.t ]
    -> ?tls_config:Tls.Config.client
    -> ?authenticator:X509.Authenticator.t
    -> ?meth:H1.Method.t
    -> ?headers:(string * string) list
    -> ?body:body
    -> ?max_redirect:int
    -> ?follow_redirect:bool
    -> ?resolver:resolver
    -> ('a, response) U.t
    -> 'a

  (** {3:example-client Examples.}

      {[
        open Vif

        (* https://raw.githubusercontent.com/<org>/<repository>/refs/heads/<branch>/README.md *)
        let readme =
          let open U in
          host "raw.githubusercontent.com"
          /% string
          /% string
          / "refs"
          / "heads"
          /% string
          / "README.md"
          /?? nil

        let get_readme ?(branch = "main") ~org ~repository () =
          C.request ~meth:`GET readme org repository branch
      ]} *)
end

module D : sig
  (** {3 Devices.}

      A device is a global instance on the http server with which a "finaliser"
      is associated. A device is available from all requests from a {!type:G.t}
      value. The same device instance is available from all domains —
      interactions with a device must therefore be {i domain-safe}.

      A device can be created from several values as well as from other devices.
      Finally, a device is constructed from an end-user value specified by
      {!val:Vif.run}. The idea is to allow the user to construct a value (from,
      for example, command line parameters) corresponding to a configuration and
      to construct these devices from this value. *)

  type ('value, 'a) arg
  type ('value, 'a) device

  type ('value, 'fn, 'r) args =
    | [] : ('value, 'value -> 'r, 'r) args
    | ( :: ) :
        ('value, 'a) arg * ('value, 'fn, 'r) args
        -> ('value, 'a -> 'fn, 'r) args

  val value : ('value, 'a) device -> ('value, 'a) arg
  val const : 'a -> ('value, 'a) arg
  val map : ('value, 'f, 'r) args -> 'f -> ('value, 'r) arg

  val device :
       name:string
    -> finally:('r -> unit)
    -> ('v, 'f, 'r) args
    -> 'f
    -> ('v, 'r) device
end

module Ds : sig
  type 'value t =
    | [] : 'value t
    | ( :: ) : ('value, 'a) D.device * 'value t -> 'value t
end

module G : sig
  type t

  val device : ('value, 'a) D.device -> t -> 'a
  (* [device w t] returns the device specified by the [w] parameter and the
     server [t]. If the latter has not been initialized via the {!val:Vif.run}
     function, the function raises an exception [Not_found]. *)
end

module Ms : sig
  type 'cfg t = [] : 'cfg t | ( :: ) : ('cfg, 'a) M.t * 'cfg t -> 'cfg t
  type ('cfg, 'v) fn = Request.request -> string -> G.t -> 'cfg -> 'v option

  val make : name:string -> ('cfg, 'v) fn -> ('cfg, 'v) M.t
end

module Status : sig
  type informational = [ `Continue | `Switching_protocols ]

  type successful =
    [ `OK
    | `Created
    | `Accepted
    | `Non_authoritative_information
    | `No_content
    | `Reset_content
    | `Partial_content ]

  type redirection =
    [ `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Temporary_redirect
    | `Use_proxy ]
  (* | `Permanent_redirect ] *)

  type client_error =
    [ `Bad_request
    | `Unauthorized
    | `Payment_required
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Not_acceptable
    | `Proxy_authentication_required
    | `Request_timeout
    | `Conflict
    | `Gone
    | `Length_required
    | `Precondition_failed
    | `Payload_too_large
    | `Uri_too_long
    | `Unsupported_media_type
    | `Range_not_satisfiable
    | `Expectation_failed
    | `Misdirected_request
    | (* | `Too_early *)
      `Upgrade_required
    | `Precondition_required
    | `Too_many_requests
    | `Request_header_fields_too_large
    | `Enhance_your_calm
    | `I_m_a_teapot ]
  (* | `Unavailable_for_legal_reasons ] *)

  type server_error =
    [ `Internal_server_error
    | `Not_implemented
    | `Bad_gateway
    | `Service_unavailable
    | `Gateway_timeout
    | `Http_version_not_supported
    | `Network_authentication_required ]

  type standard =
    [ informational | successful | redirection | client_error | server_error ]

  type t = [ standard | `Code of int ]
end

module Response : sig
  (** {3 Response.}

      A response is a construction (monad) whose initial state is {i empty}
      ({!type:e}) and must end in the state {i sent} ({!type:s}). Throughout
      this construction, the user can {!val:add}/{!val:rem}/{!val:set}
      information in the {i header}. Finally, the user must respond with content
      (via {!val:with_string}/{!val:with_stream}) and a status code. *)

  type ('p, 'q, 'a) t
  type e
  type f
  type s

  val with_source :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> string S.source
    -> (e, f, unit) t

  val with_string :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> string
    -> (e, f, unit) t

  val with_file :
       ?mime:string
    -> ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> Fpath.t
    -> (e, s, unit) t

  val with_tyxml :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> Tyxml.Html.doc
    -> (e, f, unit) t

  val respond : Status.t -> (f, s, unit) t
  (** [respond status] responds to the client with the given [status] and with
      the {i already filled} body response. *)

  (** Headers manipulation. *)

  val add : field:string -> string -> ('p, 'p, unit) t
  (** [add ~field value] adds a new [field] with the given [value] into the
      futur response. *)

  val rem : field:string -> ('p, 'p, unit) t
  (** [rem ~field value] removes a [field] from the futur response. *)

  val set : field:string -> string -> ('p, 'p, unit) t
  (** [set ~field value] sets the [field] value to the new given [value] into
      the futur response. If the [field] does not exist, [set] adds it. *)

  val add_unless_exists : field:string -> string -> ('p, 'p, bool) t
  (** [add_unless_exists ~field value] adds a new [field] with the given [value]
      into the futur response only if the given [field] does not exists yet. *)
end

module Cookie : sig
  (** {2 Cookies.}

      {!val:get} and {!val:set} are designed for round-tripping secure cookies.
      The most secure settings applicable to the current server are inferred
      automatically.

      {[
        let hello req server _ =
          let value = Cookie.get ~name:"my-cookie" server req in
          match value with
          | "ping" ->
              let* () = Cookie.set ~name:"my-cookie" server req "pong" in
              Response.respond `OK
          | "pong" ->
              let* () = Cookie.set ~name:"my-cookie" server req "ping" in
              Response.respond `OK
          | _ -> Response.respond `OK
      ]} *)

  type config

  val config :
       ?expires:float
    -> ?max_age:float
    -> ?domain:[ `host ] Domain_name.t
    -> ?path:bool
    -> ?secure:bool
    -> ?http_only:bool
    -> ?same_site:[ `Lax | `Strict | `None ]
    -> unit
    -> config

  type error = [ `Invalid_encrypted_cookie | `Msg of string | `Not_found ]

  val get :
       ?encrypted:bool
    -> name:string
    -> G.t
    -> Request.request
    -> (string, [> error ]) result
  (** [get ?encrypted ~name server req] returns the value associated to the key
      [name] from cookies. By default, cookies are encrypted. *)

  val pp_error : error Fmt.t

  val set :
       ?encrypt:bool
    -> ?cfg:config
    -> ?path:string
    -> name:string
    -> G.t
    -> ('c, 'a) Request.t
    -> string
    -> ('p, 'p, unit) Response.t
end

module Handler : sig
  type ('c, 'value) t =
       ('c, string) Request.t
    -> string
    -> G.t
    -> 'value
    -> (Response.e, Response.s, unit) Response.t option

  val static : ?top:Fpath.t -> ('c, 'value) t
end

type config
type e = Response.e
type f = Response.f
type s = Response.s

val ( let* ) :
     ('p, 'q, 'a) Response.t
  -> ('a -> ('q, 'r, 'b) Response.t)
  -> ('p, 'r, 'b) Response.t

val return : 'a -> ('p, 'p, 'a) Response.t

val config :
     ?domains:int
  -> ?cookie_key:Mirage_crypto.AES.GCM.key
  -> ?pid:Fpath.t
  -> ?http:
       [ `H1 of H1.Config.t
       | `H2 of H2.Config.t
       | `Both of H1.Config.t * H2.Config.t ]
  -> ?tls:Tls.Config.server
  -> ?backlog:int
  -> Unix.sockaddr
  -> config

val run :
     ?cfg:config
  -> ?devices:'value Ds.t
  -> ?middlewares:'value Ms.t
  -> ?handlers:('c, 'value) Handler.t list
  -> (G.t -> 'value -> (e, s, unit) Response.t) R.route list
  -> 'value
  -> unit

(**/*)

val setup_config : unit Cmdliner.Term.t
