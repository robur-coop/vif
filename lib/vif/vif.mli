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
module Stream = Stream

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
  type 'a t
  type 'a atom

  val string : string atom

  type ('a, 'b, 'c) orecord

  val record : 'b -> ('a, 'b, 'b) orecord

  type 'a field

  val field : string -> 'a atom -> 'a field
  val ( |+ ) : ('a, 'b, 'c -> 'd) orecord -> 'c field -> ('a, 'b, 'd) orecord
  val sealr : ('a, 'b, 'a) orecord -> 'a t
end

module T : sig
  type null
  type json
  type multipart_form
  type ('c, 'a) t

  val null : (null, unit) t
  val json : (json, Json.t) t
  val json_encoding : 'a Json_encoding.encoding -> (json, 'a) t
  val m : 'a Multipart_form.t -> (multipart_form, 'a) t
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

  val stream : ('c, 'a) t -> string Stream.stream
  val get : ('cfg, 'v) M.t -> ('c, 'a) t -> 'v option

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
      is associated. A device is available from all requests from a {!type:S.t}
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

module S : sig
  type t

  val device : ('value, 'a) D.device -> t -> 'a
end

module Ms : sig
  type 'cfg t = [] : 'cfg t | ( :: ) : ('cfg, 'a) M.t * 'cfg t -> 'cfg t
  type ('cfg, 'v) fn = Request.request -> string -> S.t -> 'cfg -> 'v option

  val make : name:string -> ('cfg, 'v) fn -> ('cfg, 'v) M.t
end

module Status : sig
  type t =
    [ `Accepted
    | `Bad_gateway
    | `Bad_request
    | `Code of int
    | `Conflict
    | `Continue
    | `Created
    | `Enhance_your_calm
    | `Expectation_failed
    | `Forbidden
    | `Found
    | `Gateway_timeout
    | `Gone
    | `Http_version_not_supported
    | `I_m_a_teapot
    | `Internal_server_error
    | `Length_required
    | `Method_not_allowed
    | `Misdirected_request
    | `Moved_permanently
    | `Multiple_choices
    | `Network_authentication_required
    | `No_content
    | `Non_authoritative_information
    | `Not_acceptable
    | `Not_found
    | `Not_implemented
    | `Not_modified
    | `OK
    | `Partial_content
    | `Payload_too_large
    | `Payment_required
    | `Precondition_failed
    | `Precondition_required
    | `Proxy_authentication_required
    | `Range_not_satisfiable
    | `Request_header_fields_too_large
    | `Request_timeout
    | `Reset_content
    | `See_other
    | `Service_unavailable
    | `Switching_protocols
    | `Temporary_redirect
    | `Too_many_requests
    | `Unauthorized
    | `Unsupported_media_type
    | `Upgrade_required
    | `Uri_too_long
    | `Use_proxy ]
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

  val with_stream :
       ?compression:[< `DEFLATE ]
    -> ('c, 'a) Request.t
    -> string Stream.stream
    -> (e, f, unit) t

  val with_string :
    ?compression:[< `DEFLATE ] -> ('c, 'a) Request.t -> string -> (e, f, unit) t

  val with_file :
       ?mime:string
    -> ?compression:[< `DEFLATE ]
    -> ('c, 'a) Request.t
    -> Fpath.t
    -> (e, s, unit) t

  val respond : Status.t -> (f, s, unit) t

  (** Headers manipulation. *)

  val add : field:string -> string -> ('p, 'p, unit) t
  val rem : field:string -> ('p, 'p, unit) t
  val set : field:string -> string -> ('p, 'p, unit) t
  val add_unless_exists : field:string -> string -> ('p, 'p, bool) t
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
    -> S.t
    -> Request.request
    -> (string, [> error ]) result

  val pp_error : error Fmt.t

  val set :
       ?encrypt:bool
    -> ?cfg:config
    -> ?path:string
    -> name:string
    -> S.t
    -> ('c, 'a) Request.t
    -> string
    -> ('p, 'p, unit) Response.t
end

module Handler : sig
  type ('c, 'value) t =
       ('c, string) Request.t
    -> string
    -> S.t
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
  -> (S.t -> 'value -> (e, s, unit) Response.t) R.route list
  -> 'value
  -> unit

(**/*)

val setup_config : unit Cmdliner.Term.t
