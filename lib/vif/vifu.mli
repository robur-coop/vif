module Uri : sig
  type 'a atom = 'a Tyre.t

  val int : int atom
  val string : [ `Path | `Query_value ] -> string atom
  val bool : bool atom
  val float : float atom
  val rest : string atom
  val path : string atom
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
  val keval : ?slash:bool -> ('f, 'r) t -> (string -> 'r) -> 'f
  val eval : ?slash:bool -> ('f, string) t -> 'f
end

module Json = Vif_core.Json
module Stream = Vif_core.Stream

module Headers : sig
  type t = (string * string) list

  val add_unless_exists : t -> string -> string -> t
  val get : t -> string -> string option
end

module Method : sig
  (** HTTP request methods. *)

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
  (** Type of HTTP request methods. *)
end

module Multipart_form : sig
  type 'a t
  type 'a atom

  val string : string atom
  val int : int atom

  type ('a, 'b, 'c) orecord

  val record : 'b -> ('a, 'b, 'b) orecord

  type 'a field

  val field : string -> 'a atom -> 'a field
  val ( |+ ) : ('a, 'b, 'c -> 'd) orecord -> 'c field -> ('a, 'b, 'd) orecord
  val sealr : ('a, 'b, 'a) orecord -> 'a t

  type part

  val name : part -> string option
  val filename : part -> string option
  val mime : part -> string option
  val size : part -> int option

  type stream = (part * string Stream.source) Stream.stream
end

module Type : sig
  type null
  type json
  type multipart_form
  type ('c, 'a) t

  val null : (null, unit) t
  val json : (json, Json.t) t
  val json_encoding : 'a Jsont.t -> (json, 'a) t
  val m : 'a Multipart_form.t -> (multipart_form, 'a) t
  val multipart_form : (multipart_form, Multipart_form.stream) t
  val any : ('c, string) t
end

module Middleware : sig
  type ('cfg, 'v) t
end

module Request : sig
  type ('c, 'a) t

  val target : ('c, 'a) t -> string
  val meth : ('c, 'a) t -> Method.t
  val version : ('c, 'a) t -> int
  val headers : ('c, 'a) t -> Headers.t
  val accept : ('c, 'a) t -> string list
  val of_json : (Type.json, 'a) t -> ('a, [ `Msg of string ]) result

  val of_multipart_form :
       (Type.multipart_form, 'a) t
    -> ('a, [> `Not_found of string | `Invalid_multipart_form ]) result

  val source : ('c, 'a) t -> string Stream.source
  val get : ('cfg, 'v) Middleware.t -> ('c, 'a) t -> 'v option

  type request

  val headers_of_request : request -> Headers.t
  val method_of_request : request -> Method.t
  val target_of_request : request -> string
end

module Queries : sig
  val exists : ('c, 'a) Request.t -> string -> bool
  val get : ('c, 'a) Request.t -> string -> string list
  val all : ('c, 'a) Request.t -> (string * string list) list
end

module Route : sig
  type 'r t
  type ('fu, 'return) route

  val get : ('x, 'r) Uri.t -> ((Type.null, unit) Request.t -> 'x, 'r) route
  val head : ('x, 'r) Uri.t -> ((Type.null, unit) Request.t -> 'x, 'r) route
  val delete : ('x, 'r) Uri.t -> ((Type.null, unit) Request.t -> 'x, 'r) route

  val post :
    ('c, 'a) Type.t -> ('x, 'r) Uri.t -> (('c, 'a) Request.t -> 'x, 'r) route

  val put :
    ('c, 'a) Type.t -> ('x, 'r) Uri.t -> (('c, 'a) Request.t -> 'x, 'r) route

  val ( --> ) : ('f, 'r) route -> 'f -> 'r t
end

module Device : sig
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

  val v :
       name:string
    -> finally:('r -> unit)
    -> ('v, 'f, 'r) args
    -> 'f
    -> ('v, 'r) device
end

module Devices : sig
  type 'value t =
    | [] : 'value t
    | ( :: ) : ('value, 'a) Device.device * 'value t -> 'value t
end

module Server : sig
  type t

  val device : ('value, 'a) Device.device -> t -> 'a
end

module Middlewares : sig
  type 'cfg t =
    | [] : 'cfg t
    | ( :: ) : ('cfg, 'a) Middleware.t * 'cfg t -> 'cfg t

  type ('cfg, 'v) fn =
    Request.request -> string -> Server.t -> 'cfg -> 'v option

  val v : name:string -> ('cfg, 'v) fn -> ('cfg, 'v) Middleware.t
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
  type ('p, 'q, 'a) t
  type empty
  type filled
  type sent

  val with_source :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> string Stream.source
    -> (empty, filled, unit) t

  val with_string :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> string
    -> (empty, filled, unit) t

  val with_tyxml :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> Tyxml.Html.doc
    -> (empty, filled, unit) t

  val empty : (empty, filled, unit) t
  val websocket : (empty, sent, unit) t
  val respond : Status.t -> (filled, sent, unit) t

  val redirect_to :
       ?with_get:bool
    -> ('c, 'a) Request.t
    -> ('r, (filled, sent, unit) t) Uri.t
    -> 'r

  val add : field:string -> string -> ('p, 'p, unit) t
  val rem : field:string -> ('p, 'p, unit) t
  val set : field:string -> string -> ('p, 'p, unit) t
  val add_unless_exists : field:string -> string -> ('p, 'p, bool) t
  val return : 'a -> ('p, 'p, 'a) t

  module Infix : sig
    val ( >>= ) : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  end

  module Syntax : sig
    val ( let* ) : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  end
end

module Cookie : sig
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
    -> Server.t
    -> Request.request
    -> (string, [> error ]) result

  val pp_error : error Fmt.t

  val set :
       ?encrypt:bool
    -> ?cfg:config
    -> ?path:string
    -> name:string
    -> Server.t
    -> ('c, 'a) Request.t
    -> string
    -> ('p, 'p, unit) Response.t
end

module Handler : sig
  type ('c, 'value) t =
       ('c, string) Request.t
    -> string
    -> Server.t
    -> 'value
    -> (Response.empty, Response.sent, unit) Response.t option
end

module Config : sig
  type t

  val v :
       ?cookie_key:Mirage_crypto.AES.GCM.key
    -> ?http:
         [ `Both of H1.Config.t * H2.Config.t
         | `H1 of H1.Config.t
         | `H2 of H2.Config.t ]
    -> ?tls:Tls.Config.server
    -> int
    -> t
end

val run :
     cfg:Config.t
  -> ?devices:'value Devices.t
  -> ?middlewares:'value Middlewares.t
  -> ?handlers:('c, 'value) Handler.t list
  -> Mnet.TCPv4.state
  -> (Server.t -> 'value -> (Response.empty, Response.sent, unit) Response.t)
     Route.t
     list
  -> 'value
  -> unit
