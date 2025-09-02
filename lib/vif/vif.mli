(** Vif is a simple web framework for OCaml 5 based on [httpcats] and Miou. A
    tutorial is available {{:https://robur-coop.github.io/vif/} here} to learn
    how to use it, and another tutorial is available
    {{:https://robur-coop.github.io/miou/} here} to learn how to use Miou.

    {2 Vif as a library and [vif] as a tool}

    Vif is primarily a library that can be used in a project to launch a web
    server. However, the distribution also offers a tool called [vif] that
    allows you to {b natively} interpret an OCaml script in order to launch a
    web server:

    {[
      $ cat >main.ml <<EOF
      #require "vif" ;;

      let default req server () =
        let open Vif.Response.Syntax in
        let field = "content-type" in
        let* () = Vif.Response.add ~field "text/html; charset=utf-8" in
        let* () = Vif.Response.with_string req "Hello World!" in
        Vif.Response.respond `OK
      ;;

      let routes =
        let open Vif.Uri in
        let open Vif.Route in
        [ get (rel /?? nil) --> default ]

      let () =
        Miou_unix.run @@ fun () ->
        Vif.run routes ()
      ;;
      EOF
      $ vif --pid vif.pid main.ml &
      $ hurl http://localhost:8080/ -p b
      Hello World!
      $ kill -SIGINT $(cat vif.pid)
    ]}

    {2 How to launch a Vif server?}

    The first entry point in Vif is the {!val:run} function, which launches the
    HTTP server. This function requires several arguments, the purpose of whose
    is described for the various modules that allow these arguments to be
    specified.

    Vif essentially requires a list of {i routes} for processing client
    requests. However, the user can also specify {i middleware} (see
    {!module:Middlewares}), handlers for processing requests if no routes are
    recognised (see {!module:Handler}), a function for managing the WebSocket
    protocol (see {!section:websocket}), and devices that can manage other
    protocols (such as SQL) (see {!module:Device}).

    {2 Performances & Vif}

    Vif is therefore a web server that takes advantage of OCaml 5 and the use of
    domains thanks to Miou. If you are interested in the performance that Vif
    can offer, it is intrinsic to the results obtained with [httpcats]. We
    therefore invite you to find out more about the latter if you are interested
    in these metrics.

    But here are the latest results for [httpcats] using the benchmarking
    framework provided by TechEnpower (on AMD Ryzen 9 7950X 16-Core Processor):

    {t
      | clients | threads | latencyAvg | latencyMax | latencyStdev | totalRequests |
      |--------:|--------:|:-----------|:-----------|:-------------|:--------------|
      |      16 |      16 | 47.43us    | 2.27ms     | 38.48us      | 5303700       |
      |      32 |      32 | 71.73us    | 1.04ms     | 47.58us      | 7016729       |
      |      64 |      32 | 140.29us   | 5.72ms     | 121.50us     | 7658146       |
      |     128 |      32 | 279.73us   | 11.35ms    | 287.92us     | 7977306       |
      |     256 |      32 | 519.02us   | 16.89ms    | 330.20us     | 7816435       |
      |     512 |      32 | 1.06ms     | 37.42ms    | 534.14us     | 7409781       |
    }

    The benchmark can be reproduced using
    {{:https://github.com/TechEmpower/FrameworkBenchmarks} this project}.

    There is a comparison between [httpcats] (used by Vif) using the Miou
    scheduler and Eio. For more information on this, we invite you once again to
    take a look into the [httpcats] project, which goes into detail about the
    differences between Eio and Miou.

    {2 Typed-way to make a web application}

    Vif was developed in line with the ideals of OCaml and aims to provide a
    {i typed API} for a whole range of information considered essential for web
    development. In particular, Vif offers:
    - conversion of user-submitted forms into OCaml values (such as records),
      thanks to [multipart_form]
    - transformation of JSON content into OCaml values, thanks to [jsont]
    - the ability to make {i typed} SQL queries whose results can also be
      transformed into OCaml values, thanks to [caqti] (and [caqti.template])
    - and the ability to {i type} routes, thanks to [furl]

    {2 A full OCaml implementation of a web server}

    Finally, Vif was developed as part of our cooperative
    {{:https://robur.coop} robur}, which is reimplementing a whole series of
    protocols and formats in OCaml. In this case, apart from the fact that we
    use the TCP/IP implementation provided by your system (although we are also
    developing our own, see [utcp]), everything required for Vif is strictly
    implemented in OCaml:
    - cryptographic primitives are provided by [mirage-crypto]
    - hash algorithms are provided by [digestif]
    - checksum algorithms are provided by [checkseum]
    - the TLS protocol implementation is provided by [ocaml-tls]
    - [ocaml-dns] can be used for domain name resolution
    - the {i happy-eyeballs} algorithm can be used to connect to a service which
      OCaml implementation is provided by the [happy-eyeballs] library
    - The HTTP/1.1 protocol is implemented by [ocaml-h1]
    - The H2 protocol is implemented by [h2]
    - Websockets are implemented by [ocaml-h1] (see [H1.Websocket])
    - compression ([zlib] and [gzip]) is handled by [decompress]
    - file MIME type recognition (required for [Content-Type]) is implemented by
      [conan]

    Here is an overview of Vif, its dependencies, and what this library has to
    offer for developing a web application with OCaml 5. *)

module Uri : sig
  (** The [Uri] module provides a small DSL for describing a {i format} that can
      accept values. [Uri] can be considered as the counterpart of the [Format]
      module, but for URIs. That is, you can describe a way to construct a URI
      and apply it to values such as integers or strings.

      {[
        # require "vif" ;;
        # let my_uri =
          let open Vif.Uri in
          host "robur.coop" / "repo" /% string `Path /?? any
        ;;
        val my_uri : (string -> '_w0, '_w0) -> Vif.Uri.t = <abstr>
        # let () =
          print_endline (Vif.Uri.eval my_uri "vif");
          print_endline (Vif.Uri.eval my_uri "hurl")
        ;;
        robur.coop/repo/vif
        robur.coop/repo/hurl
      ]}

      In other words, [Uri] provides a module for {i typing} URIs (and, by
      extension, the routes that can be specified to Vif). *)

  type 'a atom = 'a Tyre.t
  (** Type of a typed element that makes up the path and/or queries of a URI. *)

  val int : int atom
  (** [int] is a typed element which recognizes an integer and cast it as OCaml
      [int]. *)

  val string : [ `Path | `Query_value ] -> string atom
  (** [string where] is a typed element which recognizes a string. This element
      can be located into a {!type:path} (and, in this case, [`Path] must be
      used) or as a {!type:query} parameter of an URI (and, in this case,
      [`Query_value] must be used). *)

  val bool : bool atom
  (** [bool] is a typed element which recognizes [true] or [false] and cast it
      as an OCaml [bool]. *)

  val float : float atom
  (** [float] is a typed element which recognize a decimal number and cast it as
      an OCaml [float]. *)

  val path : string atom
  (** [path] is a typed element which recognize anything (including ['/'] and
      ['?']). It's useful for obtaining (or generating) the end/{i rest} of an
      URI. *)

  val option : 'a atom -> 'a option atom
  (** [option t] takes a typed element and make it optional. *)

  val conv : ('a -> 'b) -> ('b -> 'a) -> 'a atom -> 'b atom
  (** [conv inj prj] creates a new typed element which produces/expects an other
      typed value. [inj] describes how to cast a ['a] value to a ['b] value and
      [prj] describes how to caset a ['b] value to a ['a]. It's useful to create
      your own typed element:

      {[
        type fruit = Apple | Orange | Banana

        let fruit =
          let v =
            Tyre.regex Re.(alt [ str "apple"; str "orange"; str "banana" ])
          in
          let inj = function
            | "apple" -> Apple
            | "orange" -> Orange
            | "banana" -> Banana
            | _ -> assert false
          in
          let prj = function
            | Apple -> "apple"
            | Orange -> "orange"
            | Banana -> "banana"
          in
          Vif.Uri.conv inj prj v
      ]} *)

  type ('f, 'r) path
  (** Type of the path part of an URI. *)

  val rel : ('r, 'r) path
  (** [rel] describes the root of a URI. In this case, the URI based on [rel] is
      relative to any {i host}. *)

  val host : string -> ('r, 'r) path
  (** [host v] is a specific host (like [localhost]) that the URI based on it
      must respect (if the URI is used as route) or generate. *)

  val ( / ) : ('f, 'r) path -> string -> ('f, 'r) path
  (** [p / "foo"] operator extends a given path [p] with a new constant part
      ["foo"]. *)

  val ( /% ) : ('f, 'a -> 'r) path -> 'a atom -> ('f, 'r) path
  (** [p /% v] operator extends a given path [p] with a new {!type:atom} [v]. *)

  type ('f, 'r) query
  (** Type of the query part of an URI. *)

  val nil : ('r, 'r) query
  (** [nil] specifies that the URI has no parameters. If the URI is used to
      specify a route, this means that a request with even one parameter would
      {b not} be recognised with [nil]. If you want to recognise a URI with
      possible parameters, we recommend using {!val:any}. *)

  val any : ('r, 'r) query
  (** [any] specifies that a URI can have no parameters or multiple parameters.
      It is usually [any] that is preferred to {!val:nil} in the specification
      of a route. *)

  val ( ** ) : string * 'a atom -> ('f, 'r) query -> ('a -> 'f, 'r) query
  (** [q ** ("foo", v)] is an operator that allows you to add a new parameter
      ["foo"] with a value whose type is specified by [v] to the list of given
      parameters [q]. *)

  type ('f, 'r) t
  (** Type of an URI. *)

  val ( /? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
  (** [path /? queries] is an operator which permits to construct an URI where
      the slash at the end of the given path [path] is not required (to delimit
      queries then). *)

  val ( //? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
  (** [path //? queries] is an operator which permits to construct an URI where
      the slash at theend of the given path [path] {b is required} and delimit
      queries then. *)

  val ( /?? ) : ('f, 'x) path -> ('x, 'r) query -> ('f, 'r) t
  (** [path /?? queries] is an operator which permits to construct an URI where
      the slash is {b optionnal} between the given path [path] and queries. *)

  val keval : ?slash:bool -> ('f, 'r) t -> (string -> 'r) -> 'f
  (** [keval ?slash uri fn] compiles an URI [uri] into a [string] and pass it to
      [fn]. *)

  val eval : ?slash:bool -> ('f, string) t -> 'f
  (** [eval ?slash uri] compiles an URI [uri] into a [string]. *)
end

module Json = Json
module Stream = Vif_stream

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
  (** Vif proposes a way to describe a form via types in order to decode a
      [multipart/form-data] request and obtain an OCaml record.

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
          let open Vif.Uri in
          let open Vif.Route in
          let open Vif.Type in
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

  (** {3:multipart-stream Streaming API of [multipart/form-data] requests.}

      The user may want to manage a request containing a form in the form of a
      stream. This is particularly useful if you want to upload a file (and,
      instead of storing it in memory, directly write the received file to a
      temporary file). *)

  type part
  (** Type of a part from the [multipart/form-data] stream.

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

  type stream = (part * string Stream.source) Stream.stream
  (** Type of a [multipart/form-data] stream.

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

module Type : sig
  (** {3:type-of-requests Type of requests.}

      Vif is able to dispatch requests not only by the route but also by the
      type of content of the request (given by the ["Content-Type"]). These
      values represent a certain type such as the type "application/json" or
      "multipart/form-data". These last two can be completed by an "encoding"
      making it possible to transform the content of the requests into an OCaml
      value. *)

  type null
  (** Type of empty body requests. *)

  type json
  (** Type of JSON requests ([application/json]). *)

  type multipart_form
  (** Type of [multipart/form-data] requests. *)

  type ('c, 'a) t
  (** Type to describe the body of requests. *)

  val null : (null, unit) t
  val json : (json, Json.t) t

  val json_encoding : 'a Jsont.t -> (json, 'a) t
  (** [json_encoding t] is an [application/json] request whose content is a JSON
      value that complies with the given format [t]. *)

  val m : 'a Multipart_form.t -> (multipart_form, 'a) t
  (** [m t] is a [multipart/form-data] request whose content is a value that
      complies with the given format [t]. *)

  val multipart_form : (multipart_form, Multipart_form.stream) t
  (** [multipart_form] is a [multipart/form-data] request whose content can be
      consumed as a {i stream} (see {!section:multipart-stream}) *)

  val any : ('c, string) t
  (** [any] allows requests to be accepted without filtering by [Content-Type].
      The content is interpreted as a [string]. *)
end

module Middleware : sig
  type ('cfg, 'v) t
  (** Type of middlewares. *)
end

module Request : sig
  type ('c, 'a) t
  (** Type of a request. *)

  val target : ('c, 'a) t -> string
  (** [target req] corresponds to the path requested by the request [req]. The
      invariant is that it always starts with a slash ['/'] .*)

  val meth : ('c, 'a) t -> Method.t
  (** [meth req] is the method (see {!module:Method}) of the given request
      [req]. *)

  val version : ('c, 'a) t -> int
  (** [version req] is the HTTP version used to communicate. *)

  val headers : ('c, 'a) t -> Headers.t
  (** [headers req] returns headers (see {!module:Headers}) of the given request
      [req]. *)

  val accept : ('c, 'a) t -> string list
  (** [accept req] returns what the client can accept and understand. *)

  val of_json : (Type.json, 'a) t -> ('a, [ `Msg of string ]) result

  val of_multipart_form :
       (Type.multipart_form, 'a) t
    -> ('a, [> `Not_found of string | `Invalid_multipart_form ]) result

  val source : ('c, 'a) t -> string Stream.source

  val get : ('cfg, 'v) Middleware.t -> ('c, 'a) t -> 'v option
  (** [get middleware req] returns the value optionally added by the given
      [middleware] and the request [req]. *)

  (** {3:request-middleware Requests for middlewares}

      As soon as it comes to executing the various middlewares
      ({!type:Middleware.t}) defined by the user, the latter can manipulate the
      HTTP request given by the client. However, the latter has a limitation:
      the body of the request {b cannot} be obtained from the {!type:request}
      type of value. Indeed, middlewares should not manipulate the body of
      requests and should only refer to meta-data (such as
      {{!val:headers_of_request} headers}). *)

  type request
  (** Type of a request (in the view of a middleware). *)

  val headers_of_request : request -> Headers.t
  (** [headers_of_request] is the headers (see {!module:Headers}) of the given
      request. *)

  val method_of_request : request -> Method.t
  (** [method_of_request] is the method (see {!module:Method}) of the given
      request. *)

  val target_of_request : request -> string
  (** [target_of_request] is the path of the given request. *)
end

module Queries : sig
  (** A request not only has content but also parameters ({i queries}) that can
      be specified via the URI. This module allows you to extract the values
      specified in the URI to the user when managing the request. *)

  val exists : ('c, 'a) Request.t -> string -> bool
  (** [exists req query] checks whether the [query] information has been
      provided by the user via the URI. *)

  val get : ('c, 'a) Request.t -> string -> string list
  (** [get req query] returns the values associated with the key [query] given
      in the URI. *)

  val all : ('c, 'a) Request.t -> (string * string list) list
  (** [all req] returns all the query parameters as an associative list. *)
end

module Route : sig
  type 'r t
  type ('fu, 'return) route

  val get : ('x, 'r) Uri.t -> ((Type.null, unit) Request.t -> 'x, 'r) route
  (** [get uri] describes a route which matches a [GET] request with the given
      path [uri]. a [GET] request does not have any contents. *)

  val head : ('x, 'r) Uri.t -> ((Type.null, unit) Request.t -> 'x, 'r) route
  (** [head uri] describes a route which matches a [HEAD] request with the given
      path [uri]. A [HEAD] request does not have any contents. *)

  val delete : ('x, 'r) Uri.t -> ((Type.null, unit) Request.t -> 'x, 'r) route
  (** [delete uri] describes a route which matches a [DELETE] request with the
      given path [uri]. A [DELETE] request does not have any contents. *)

  val post :
    ('c, 'a) Type.t -> ('x, 'r) Uri.t -> (('c, 'a) Request.t -> 'x, 'r) route

  val put :
    ('c, 'a) Type.t -> ('x, 'r) Uri.t -> (('c, 'a) Request.t -> 'x, 'r) route

  val ( --> ) : ('f, 'r) route -> 'f -> 'r t
  (** [r --> f] associates a route [r] to a handler [f]. *)
end

module Client : sig
  (** Module [Client] implements the {b c}lient part of the HTTP protocol. *)

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
    -> ('a, response) Uri.t
    -> 'a

  (** {3:example-client Examples.}

      {[
        (* https://raw.githubusercontent.com/<org>/<repository>/refs/heads/<branch>/README.md *)
        let readme =
          let open Vif.Uri in
          host "raw.githubusercontent.com"
          /% string `Path
          /% string `Path
          / "refs"
          / "heads"
          /% string `Path
          / "README.md"
          /?? nil

        let get_readme ?(branch = "main") ~org ~repository () =
          Vif.Client.request ~meth:`GET readme org repository branch
      ]} *)
end

module Device : sig
  (** {3 Devices.}

      A {i device} is a global instance that Vif initialises at the same time as
      the HTTP server. The purpose of a {i device} is to access another global
      {i resource} that is not related to HTTP things but is necessary to
      implement certain logic within your request handlers.

      An example of devices is a {i connection pool} to a SQL server to enable
      our request handlers to communicate with it. Instead of initiating a
      connection to the database for each request within our request handlers,
      it is possible to initialise a connection pool when the Vif server is
      launched, which can then be used by our request handlers {b in parallel}.
      It is therefore a {b global} resource that can be retrieved within request
      handlers using {!val:Server.get}. Here is an example of a [caqti]
      {i device} (used to communicate with an SQL server):

      {[
        type cfg = { sw: Caqti_miou.Switch.t; uri: Uri.t }

        let caqti =
          let finally (module Conn : Caqti_miou.CONNECTION) =
            Conn.disconnect ()
          in
          Vif.Device.v ~name:"caqti" ~finally [] @@ fun { sw; uri } ->
          match Caqti_miou_unix.connect ~sw uri with
          | Ok conn -> conn
          | Error err ->
              Logs.err (fun m -> m "%a" Caqti_error.pp err);
              Fmt.failwith "%a" Caqti_error.pp err

        let () =
          Miou_unix.run @@ fun () ->
          Caqti_miou.Switch.run @@ fun sw ->
          let uri = Uri.of_string "sqlite3:foo.sqlite?create=false" in
          let cfg = { sw; uri } in
          Vif.run ~devices:Vif.Devices.[ caqti ] routes cfg
      ]} *)

  type ('value, 'a) arg
  (** Type of an argument to initialize a device. *)

  type ('value, 'a) device
  (** Type of a device. *)

  type ('value, 'fn, 'r) args =
    | [] : ('value, 'value -> 'r, 'r) args
    | ( :: ) :
        ('value, 'a) arg * ('value, 'fn, 'r) args
        -> ('value, 'a -> 'fn, 'r) args

  val value : ('value, 'a) device -> ('value, 'a) arg
  (** [value device] describes the given device [device] as an argument to
      initialize another device (see {!val:v}). *)

  val const : 'a -> ('value, 'a) arg
  (** [const v] gives a constant value to the initializer of a device (see
      {!val:v}). *)

  val map : ('value, 'f, 'r) args -> 'f -> ('value, 'r) arg
  (** [map args fn] describes a function which requires few arguments and return
      a value which will be used by the initializer of a new device (see
      {!val:v}). *)

  val v :
       name:string
    -> finally:('r -> unit)
    -> ('v, 'f, 'r) args
    -> 'f
    -> ('v, 'r) device
  (** [v ~name ~finally args fn] creates a new {i device} which can be
      initialized and finalized by Vif. A device can require the result of some
      other devices. In that case, {!val:value} is used to pass required devices
      to the initializer of the new device. A device can also require constant
      values. In that case, {!val:const} is used to pass as an argument the
      value to the initializer of the new device. Finally, the device can
      require a final abstract value that corresponds to the value given to
      {!val:run}.

      From the {!type:Server.t} value, the user can retrieve the device in
      request handlers and middleware via {!val:Server.device}.

      {b NOTE}: There cannot be a cyclic dependency between devices, i.e. device
      {i a} cannot depend on device {i b} that depends on device {i a}. In this
      case, Vif will fail with an error. *)
end

module Devices : sig
  type 'value t =
    | [] : 'value t
    | ( :: ) : ('value, 'a) Device.device * 'value t -> 'value t
end

module Server : sig
  type t

  val device : ('value, 'a) Device.device -> t -> 'a
  (* [device w t] returns the device specified by the [w] parameter and the
     server [t].

     @raise Not_found if the given device was not initialized via the
       {!val:Vif.run} function. *)
end

module Middlewares : sig
  (** {3:middlewares Middlewares.}

      Middleware is a function {!type:fn} that applies to all requests. The user
      can introspect the headers (and only the headers) of the requests in order
      to add information (such as the connected user if a field in the headers
      provides such information). This information added to the request can be
      retrieved from the request handlers via {!val:Request.get}.

      Here is an example of middleware confirming client authentication via the
      ["Authorization"] field:

      {[
        let ( let* ) = Option.bind

        let decode str =
          match String.split_on_char ' ' str with
          | [ "Basic"; b64 ] ->
              let data = Base64.decode b64 in
              let* data = Result.to_option data in
              let data = String.split_on_char ':' data in
              let username = List.hd data and password = List.tl data in
              let password = String.concat ":" password in
              Some (username password)
          | _ -> None

        let auth =
          VIf.Middlewares.v ~name:"auth" @@ fun req _target _server _ ->
          let hdrs = Vif.Request.headers_of_request req in
          let* value = Vif.Headers.get hdrs "Authorization" in
          let* username, password = decode value in
          Some (username, password)

        let () =
          Miou_unix.run @@ fun () ->
          let middlewares = Vif.Middlewares.[ auth ] in
          Vif.run ~middlewares routes ()
      ]} *)

  type 'cfg t =
    | [] : 'cfg t
    | ( :: ) : ('cfg, 'a) Middleware.t * 'cfg t -> 'cfg t

  type ('cfg, 'v) fn =
    Request.request -> string -> Server.t -> 'cfg -> 'v option
  (** Type of function which implements a middleware. *)

  val v : name:string -> ('cfg, 'v) fn -> ('cfg, 'v) Middleware.t
  (** [make ~name fn] creates a new {i middleware} which can be used by the
      server (you must specify the {i witness} returned by this function into
      {!val:run}).

      Once all middlewares has been executed on the incoming request, it is
      possible to obtain the values calculated by these middlewares using
      {!val:Request.get}. *)
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

      A response is a construction ({i monad}) whose initial state is
      {!type:empty} and must end in the state {!type:sent}. Throughout this
      construction, the user can {!val:add}/{!val:rem}/{!val:set} information in
      the {i headers}. Finally, the user must respond with content (via
      {!val:with_string}/{!val:with_stream}) and a status code.

      The interest behind this monad with preconditions and postconditions is to
      enforce the developer to respond only once to the client. This monad also
      enforces that the developer {b must} respond—in fact, the only way to not
      respond in a request handler is to raise an exception. *)

  type ('p, 'q, 'a) t
  (** Type of the response monad. *)

  type empty
  (** The [empty] state is a response that does not yet have any content. The
      user can transition from this state to the {!type:filled} state using
      functions such as {!val:with_string} or {!val:with_tyxml}. *)

  type filled
  (** The [filled] state is a response that already has associated content (a
      stream, a string, etc.). The user can still manipulate the response, such
      as modifying its headers, but can no longer modify the content of the
      response. However, the user can transition to the {!type:sent} state using
      the {!val:respond} function. *)

  type sent
  (** The [sent] state is the final state of a response and informs the user
      that the response has been sent to the client. After this state, any
      post-modification of the response (including its headers) is {b useless}.
  *)

  val with_source :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> string Stream.source
    -> (empty, filled, unit) t
  (** [with_string req src] responds the given stream [src] to the client. *)

  val with_string :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> string
    -> (empty, filled, unit) t

  val with_file :
       ?mime:string
    -> ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> Fpath.t
    -> (empty, sent, unit) t

  val with_tyxml :
       ?compression:[> `DEFLATE | `Gzip ]
    -> ('c, 'a) Request.t
    -> Tyxml.Html.doc
    -> (empty, filled, unit) t
  (** [with_tyxml req tyxml] responds an HTML contents [tyxml] to the client. *)

  val empty : (empty, filled, unit) t
  (** [empty] fills the current response without contents. *)

  val websocket : (empty, sent, unit) t
  (** [websocket] upgrades the current connection to the websocket protocol. The
      [websocket] handler specified into {!val:run} takes over and manages this
      connection according to the WebSocket protocol. *)

  val respond : Status.t -> (filled, sent, unit) t
  (** [respond status] responds to the client with the given [status] and with
      the {i already filled} body response. *)

  val redirect_to :
       ?with_get:bool
    -> ('c, 'a) Request.t
    -> ('r, (filled, sent, unit) t) Uri.t
    -> 'r
  (** [redirect_to ?with_get req uri] responds a redirection to [uri] to the
      client. *)

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
      into the futur response only if the given [field] {b does not} exists yet.
  *)

  val return : 'a -> ('p, 'p, 'a) t
  (** [return v] fullfills the construction with a value but it {b does not}
      change the current state of the {i monad}. *)

  module Infix : sig
    val ( >>= ) : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  end

  module Syntax : sig
    val ( let* ) : ('p, 'q, 'a) t -> ('a -> ('q, 'r, 'b) t) -> ('p, 'r, 'b) t
  end
end

module Cookie : sig
  (** {2 Cookies}

      {!val:get} and {!val:set} are designed for round-tripping secure cookies.
      The most secure settings applicable to the current server are inferred
      automatically.

      {[
        let hello req server _ =
          let open Vif.Response.Syntax in
          let value = Vif.Cookie.get ~name:"my-cookie" server req in
          match value with
          | "ping" ->
              let* () = Vif.Cookie.set ~name:"my-cookie" server req "pong" in
              Vif.Response.respond `OK
          | "pong" ->
              let* () = Vif.Cookie.set ~name:"my-cookie" server req "ping" in
              Vif.Response.respond `OK
          | _ -> Vif.Response.respond `OK
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
    -> Server.t
    -> Request.request
    -> (string, [> error ]) result
  (** [get ?encrypted ~name server req] returns the value associated to the key
      [name] from cookies. By default, cookies are encrypted. *)

  val pp_error : error Fmt.t
  (** Pretty printer of {!type:error}s. *)

  val set :
       ?encrypt:bool
    -> ?cfg:config
    -> ?path:string
    -> name:string
    -> Server.t
    -> ('c, 'a) Request.t
    -> string
    -> ('p, 'p, unit) Response.t
  (** [set ?encrypt ?cfg ?path ~name server req value] creates a new cookie
      [name] on the client side with the value [value] that can be retrieved
      with {!val:get} afterwards. By default, the cookie is encrypted. *)
end

module Handler : sig
  (** The user may want more precise dispatching than Vif can offer. In this
      case, if Vif cannot find any routes for a given request, handlers are used
      to possibly provide a response. Handlers therefore allow you to handle
      cases that cannot be described using Vif's URI and route system. *)

  type ('c, 'value) t =
       ('c, string) Request.t
    -> string
    -> Server.t
    -> 'value
    -> (Response.empty, Response.sent, unit) Response.t option
  (** The type of handlers.

      The handler processes the request and can extract its content as a
      [string]. It also obtains the {i path}/{i target} specified in the request
      and, like typed handlers, has a value representing the server and the
      value given to {!val:run}. *)

  val static : ?top:Fpath.t -> ('c, 'value) t
end

type config

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

type ic = Httpcats.Server.Websocket.ic
type oc = Httpcats.Server.Websocket.oc

val run :
     ?cfg:config
  -> ?devices:'value Devices.t
  -> ?middlewares:'value Middlewares.t
  -> ?handlers:('c, 'value) Handler.t list
  -> ?websocket:(ic -> oc -> Server.t -> 'value -> unit)
  -> (Server.t -> 'value -> (Response.empty, Response.sent, unit) Response.t)
     Route.t
     list
  -> 'value
  -> unit

(**/*)

val setup_config : unit Cmdliner.Term.t
