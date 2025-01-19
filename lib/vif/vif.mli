module U : sig
  type 'a atom = 'a Tyre.t
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

module R : sig
  type 'r route

  val route : ('f, 'r) U.t -> 'f -> 'r route
  val ( --> ) : ('f, 'r) U.t -> 'f -> 'r route
  val dispatch : default:(string -> 'r) -> 'r route list -> target:string -> 'r
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
          /% Tyre.string
          /% Tyre.string
          / "refs"
          / "heads"
          /% Tyre.string
          / "README.md"
          /?? nil

        let get_readme ?(branch = "main") ~org ~repository () =
          C.request ~meth:`GET readme org repository branch
      ]} *)
end

module D : sig
  type 'a arg
  type 'a device

  type ('f, 'r) args =
    | [] : ('r, 'r) args
    | ( :: ) : 'a arg * ('f, 'a -> 'r) args -> ('f, 'r) args

  val const : 'a -> 'a arg
  val map : ('f, 'r) args -> 'f -> 'r arg

  val device :
       name:string
    -> finally:('r -> unit)
    -> ('f, 'r) args
    -> 'f
    -> 'r arg * 'r device

  (** Some devices. *)

  val rng : Mirage_crypto_rng_miou_unix.rng arg
end

module S : sig
  type t
  type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]

  val reqd : t -> reqd
  val device : 'a D.device -> t -> 'a
  val rng : Mirage_crypto_rng_miou_unix.rng D.device
end

module Stream : sig
  type 'a t

  val create : int -> 'a t
  val put : 'a t -> 'a -> unit
  val get : 'a t -> 'a option
  val close : 'a t -> unit
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

module Headers : sig
  type t = (string * string) list
end

module Request : sig
  type t

  val target : t -> string
  val meth : t -> Method.t
  val version : t -> int
  val headers : t -> Headers.t
  val to_string : t -> string
  val to_stream : t -> string Stream.t
end

module Response : sig
  type t

  val with_stream :
    S.t -> ?headers:Headers.t -> Status.t -> (string Stream.t -> unit) -> unit

  val with_string : S.t -> ?headers:Headers.t -> Status.t -> string -> unit
end

type config
and stop

val config :
     ?http:
       [ `H1 of H1.Config.t
       | `H2 of H2.Config.t
       | `Both of H1.Config.t * H2.Config.t ]
  -> ?tls:Tls.Config.server
  -> ?backlog:int
  -> ?stop:stop
  -> Unix.sockaddr
  -> config

val stop : unit -> stop

type devices = [] : devices | ( :: ) : 'a D.arg * devices -> devices

val run :
     cfg:config
  -> devices:devices
  -> default:(string -> S.t -> Request.t -> 'value -> unit)
  -> (S.t -> Request.t -> 'value -> unit) R.route list
  -> 'value
  -> unit
