# My basic userspace

Vif implements the basics of web frameworks. It offers middleware and cookie
features, among others. In this chapter, we will use Vif's routing system to
see how to implement a user space. We will essentially create two routes:
- a route that allows users to submit their login details
- a route that should only be accessible to users

As for the first route, since it involves submitting identifiers, it will be a
POST route. In the previous chapter, it was possible to _type_ routes, but Vif
is also capable of typing the content of requests. In this case, we will create
a POST route that expects content in JSON format (which will be the user's
identifiers).

## `jsont` & Vif

Vif uses the [`jsont`][jsont] library to obtain a DSL for describing
information in JSON format. The aim here is to describe a type (the
identifiers) and describe its equivalent in JSON format using `jsont`.

```ocaml
type credentials =
  { username : string 
  ; password : string }
;;

let credentials =
  let open Jsont in
  let username = Object.mem "username" string in
  let password = Object.mem "password" string in
  let fn username password =
    { username; password } in
  Object.map fn
  |> username
  |> password
  |> Object.finish
;;
```

Thanks to the `credentials` value, we can now serialise and deserialise JSON
and obtain an OCaml value of type `credentials`. Vif is capable of handling
this type of value in order to deserialise the content of a request itself as
soon as it recognises the content type `application/json`.

```ocaml
let login req _server () =
  let open Vif.Response.Syntax in
  match Vif.Request.of_json req with
  | Ok (v : credentials) ->
    let str = Fmt.str "username: %S, password: %S\n"
      v.username v.password in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | Error (`Msg msg) ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid JSON: %s\n" msg in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (json_encoding credentials) (rel /?? nil) --> login ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes () ;;
```

Here's how to send a JSON request using `hurl`:

```shell
$ hurl -m POST http://localhost:8080/ username=robur password=42 -p b
username: "robur", password: "42"
$ hurl -m POST http://localhost:8080/ foo=bar
HTTP/1.1 422 

connection: close
content-length: 122
content-type: text/plain; charset=utf-8

Invalid JSON: Missing members in object:
 password
 username
```

In this example, we have specified that our route expects content in JSON
format. Next, Vif uses `jsont` to attempt to deserialise the given JSON to the
OCaml value `credentials`. If it fails, we return code 422; otherwise, we
display the information.

The goal here is that serialisation and deserialisation can be seen as a fairly
repetitive task for the user. In this case, Vif handles two formats: JSON using 
`jsont` and `multipart/form-data`, which we will see in another chapter. These
are the two most commonly used formats with the HTTP protocol, and Vif therefore
handles these formats _natively_, providing you with a simple way to deserialise
them into OCaml values.

## Vif and cookies

When a user logs in, we want to keep the information on the user's side so that
they remain logged in. We therefore want to store this information and also
secure it. We will use Vif to create a new cookie, which will contain a JSON
Web Token ([JWT][jwt]) that ensures the information is secure.

The JWT requires a _secret_, a value that only the server knows in order to
encrypt/decrypt the JWT. This is where we introduce three concepts:
- Vif's ability to load an external library (we will use [jwto][jwto], an
  implementation of JWT in OCaml)
- the ability to obtain configuration values (often found in the `.env` file of
  a website)
- the creation of a new cookie if the user has logged in successfully

```ocaml
#require "jwto" ;;

type env =
  { secret : string }
;;

let verify { username; password } =
  match username, password with
  | "robur", "42" -> true
  | _ -> false
;;

let login req server { secret } =
  let open Vif.Response.Syntax in
  match Vif.Request.of_json req with
  | Ok ({ username; _ } as v) when verify v ->
    let token = Jwto.encode HS512 secret [ "username", username ] in
    let token = Result.get_ok token in
    let* () = Vif.Cookie.set ~name:"my-token" server req token in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Authenticated!\n" in
    Vif.Response.respond `OK
  | Ok _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Error (`Msg msg) ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid JSON: %s\n" msg in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (json_encoding credentials) (rel / "login" /?? nil) --> login ]
;;

let () = Miou_unix.run @@ fun () ->
  let env = { secret= "deadbeef" } in
  Vif.run routes env ;;
```

As you can see, it is quite simple to load a new library; just use the
`require` directive. Next, define a new `env` type that will contain the secret
needed to generate JWTs.

We implement a fairly simple function `verify` that recognises a single user
with their password.

Finally, we modify the `login` function to verify the information provided by
the client. If the credentials are correct, we generate a JWT token using our
`secret` passed to our `login` function and use `Vif.Cookie.set` to add a new
cookie.

This is where you can specify the last argument of the `Vif.run` function. This
value will be passed to all your _handlers_. Its main purpose is to allow
developers to define their own type containing global information such as the
secret used to generate JWTs. Of course, you can extend the `env` type. It is
often compared to `.env` found in several web frameworks.

This is now the result when attempting to log in:

```shell
$ hurl -m POST http://localhost:8080/login username=robur password=21 -p rb
HTTP/1.1 401 Unauthorized

Unauthorized!
$ hurl -m POST http://localhost:8080/login username=robur password=42
HTTP/1.1 200 OK

connection: close
content-length: 15
content-type: text/plain; charset=utf-8
set-cookie: __Host-my-token=ALeeJoP8W2KfmX9oYcHMnjeJDuGJmV67brUluoEJgHLZWHEk...
  Path=/; Secure; HttpOnly; SameSite=Lax

Authenticated!
```

We are well connected! We can clearly see the cookie that the client should
save. If we take a step back from the code, what we are doing here is managing
a POST request containing identifiers, checking that they are correct, and
creating a JWT using `jwto` if they are. In this token, we will simply store
the user's `username`. We then need to inform the client that we would like to
save this token on their end. We do this using `Vif.Cookie.set` (there are a
whole host of options, but I'll leave you to discover them in the
documentation).

As a client, we can see the cookie (with `Set-Cookie`). It is encrypted...
twice! Once by Vif itself (you can — and should — specify the encryption key
with Vif.config) and once by `jwto`.

We can now introduce a new concept in Vif: _middleware_.

## Vif and middleware

Middleware is a simple function that applies to all requests (whether or not
there is a defined route). Within this function, it is possible to inspect the
request, particularly the _header_. However, it is impossible to:
1) inspect the content of the request
2) send a response

The purpose of middleware is to transform and/or add information from the
header of the incoming request. For example, it is possible to analyse the
cookies sent by the user and determine whether or not they were able to log in.
Finally, this information is added to the request.

After the middleware, the handler associated with the route can finally process
the request to which the middleware has added information. This information can
be retrieved and a response can be sent accordingly.

The goal here would be to have middleware that attempts to deserialise the JWT
present in the cookies. If it succeeds, it means that the person has logged in
previously. The user's `username` is included in this token, which allows us to
identify them. Here is how to create middleware using Vif.

```ocaml
type user =
  { username : string }
;;

let jwt =
  Vif.Middleware.v ~name:"jwt" @@ fun req target server { secret } ->
  match Vif.Cookie.get server req ~name:"my-token" with
  | Error _err -> None
  | Ok token ->
    let ( let* ) = Option.bind in
    let token = Jwto.decode_and_verify secret token in
    let* token = Result.to_option token in
    let* username = List.assoc_opt "username" (Jwto.get_payload token) in
    Some { username }
;;

let index req server _env =
  let open Vif.Response.Syntax in
  match Vif.Request.get jwt req with
  | None ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Some { username } ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Connected as %S!\n" username in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (json_encoding credentials) (rel / "login" /?? nil) --> login
  ; get (rel /?? nil) --> index ]
;;

let () = Miou_unix.run @@ fun () ->
  let env = { secret= "deadbeef" } in
  let middlewares = Vif.Middlewares.[ jwt ] in
  Vif.run ~middlewares routes env ;;
```

Here, we create a new middleware called `jwt`. Its purpose is to search for a
cookie called `my-token` (the one we created in the `login` handler) and decode
(and verify) the JWT. If successful, we should obtain the username of the
current user. We will simply add this information to the current request and
return `Some <data>`.

In our new `index` handler, we will try to see if our middleware has returned
the information correctly. If so, it means that the client has a JWT and is
logged in. All we need to do is display the username!

Finally, we need to tell Vif that we want to apply this middleware.

Note that the middleware also has access to our `env` and therefore also has
access to our `secret` value (required for `jwto`). The user can also specify
the type of value that the middleware is capable of creating. In our case, we
have defined the type `user` containing only the field `username`. We can of
course extend this type with other values such as the user's ID, age, etc. In
short, Vif allows you to define your own types.

You can now test our website and its user area:

```shell
$ curl -X POST http://localhost:8080/login \
  --header 'Content-Type: application/json' \
  --data "{ \"username\": \"robur\", \"password\": \"42\" }" \
  --cookie-jar cookies.txt
Authenticated!
$ curl http://localhost:8080/ --cookie cookies.txt
Connected as "robur"!
$ curl http://localhost:8080/
Unauthorized!
```

Et voilà! A user space created using Vif. This allowed us to explore the
concepts of cookies, JWTs, and middleware.

At this stage, we have most of what a framework can offer. We can manage
incoming information (and type it) as specified in our routes or in the request
content, and process this information while allowing developers to specify
their own types.

The main idea is that OCaml, being a typed language, has the advantage of
characterising information (at least, much more than a simple `string`). We can
use ADTs or records, which are much easier to use. It is then essentially a
matter of defining how to convert untyped information into OCaml values.

## Next steps

We now have a solid foundation for improving our website. In the next chapter,
we will propose a way to display an HTML page for logging in, which will allow
us to introduce a new format: `multipart/form-data`. Next, we will see how to
generate HTML dynamically depending on whether the user is logged in or not.
This will allow us to introduce new concepts such as `ppx` and static files.

[jsont]: https://github.com/dbuenzli/jsont
[jwt]: https://en.wikipedia.org/wiki/JSON_Web_Token
[jwto]: https://github.com/sporto/jwto
