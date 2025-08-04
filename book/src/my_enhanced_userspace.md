# My enhanced userspace

We have seen the basic elements for our user space. However, our website is
still quite limited, as it assumes that the user knows how to send a POST
request via the command line. In this chapter, we will improve our user space
so that:
- we have an HTML page with a login form
- we can handle the `multipart/form-data` format rather than the JSON format
- we have a dynamic HTML page depending on whether the user is logged in or not

## Static HTML files (and [`conan`][conan])

When creating a website, we often want to deliver static content such as images
or HTML pages. When sending these documents to the client, important
information is sent along with them: the [MIME type][mime-type].

This informs the user of the type of content being sent (e.g. whether it is a
`*.png` or `*.jpg` image).

With this in mind, we have developed software that can recognise the MIME type
of any file: the `conan` project. Recognition is not based on the file
extension, but rather on the content and a database that references most MIME
types.

```shell
$ conan.file --mime login.html
text/html
```

Next, for Vif, we need to add a handler that, depending on the path given by
the request, not only attempts to find the associated static file, but also
recognises its MIME type using `conan` to deliver it to the client. Let's start
with this HTML form:
```xml
<html>
<body>
<form action="/login" method="post" enctype="multipart/form-data">
  <label for="username">Enter your username: </label>
  <input type="text" name="username" required />
  <label for="password">Enter your password: </label>
  <input type="password" name="password" required />
  <input type="submit" value="Login!" />
</form>
</body>
</html>
```

Since the route for delivering a static file depends on the filename, we will
introduce a new concept to Vif: _default handlers_. When Vif cannot find any
routes for a request, it will execute a series of default handlers until one
produces a response.

For static files, Vif already provides such a handler: `Vif.Handler.static`.
Just add it to `Vif.run`:

```ocaml
let () = Miou_unix.run @@ fun () ->
  let env = { secret= "deadbeef" } in let middlewares = Vif.Middlewares.[ jwt ]
in let handlers = [ Vif.Handler.static ?top:None ] in Vif.run ~handlers
~middlewares routes env ;;
```

The `top` value corresponds to the location of the static files (so that Vif
prohibits access to other files deeper in the directory tree). In addition, our
`login.html` file is in the same folder as our `main.ml` file (however, it is
recommended to create an appropriate folder containing only static files).

```shell
$ hurl http://localhost:8080/login.html
HTTP/1.1 200 OK

transfer-encoding: chunked
etag: 3b8eae63b7baa4a7c24bfd8ee7600ee4b97306064e9ea336fca949011058a559
content-length: 356
content-type: text/html

<html>
<body>
<form action="/login" method="post" enctype="multipart/form-data">
  <label for="username">Enter your username: </label>
  <input type="text" name="username" required />
  <label for="password">Enter your password: </label>
  <input type="password" name="password" required />
  <input type="submit" value="Login!" />
</form>
</body>
</html>
```

We are now able to deliver static files! Note the appearance of `ETag`. This is
information that allows the client to cache the file (and avoid re-downloading
the content). Finally, note that `conan` has correctly recognised the MIME type
of our file.

### MIME-type and `conan`

MIME file recognition is quite difficult and is a topic in itself (in terms of
performance and ability to recognise strange files). Even though `conan` finds
quite a few solutions, it may happen that we are unable to recognise the file
type. There are other, less obvious ways to transfer the contents of a file
using Vif (such as `Vif.Response.with_file`), where you can specify the MIME
type manually.

It is also possible to improve `conan` (and its database) to recognise a subset
or larger set of files. We will leave it up to the user to choose the best
solution for their context.

## Vif & `multipart/form-data`

In our form, we specify that we would like to transfer the information in
`multipart/form-data` format. This is a somewhat special format that is often
used for websites. Fortunately, there is an implementation in OCaml that Vif
uses: [multipart_form][multipart_form].

Vif extends this library so that it is as easy to use as `jsont`: we therefore
offer another DSL for describing the format of your forms:

```ocaml
let login_form =
  let open Vif.Multipart_form in
  let fn username password =
    { username; password } in
  record fn
  |+ field "username" string
  |+ field "password" string
  |> sealr
;;

let login req server { secret } =
  let open Vif.Response.Syntax in
  match Vif.Request.of_multipart_form req with
  | Ok ({ username; _ } as v) when login v ->
    let token = Jwto.encode HS512 secret [ "username", username ] in
    let token = Result.get_ok token in
    let str = "Authenticated!\n" in
    let* () = Vif.Cookie.set ~name:"my-token" server req token in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | Ok _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Error _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid multipart/form-data\n" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  let open Vif.Type in
  [ post (m login_form) (rel / "login" /?? nil) --> login
  ; get (rel /?? nil) --> index ]
;;
```

The idea is almost the same as with `jsont`. We just need to redefine our login
route with `m` instead of `json_encoding` so that Vif accepts requests with a
`Content-Type` of `multipart/form-data`.

```shell
$ hurl -m POST http://localhost:8080/login --multipart username=robur password=42 -p b
Authenticated!
```

## Vif, `tyxml` and `ppx`

It would now be more useful to transmit HTML content rather than simple text.
Several solutions are available, but we will use one in particular that
emphasises the typed generation of an HTML document using [TyXML][tyxml]:

```ocaml
#require "tyxml-ppx" ;;
#require "tyxml" ;;

open Tyxml ;;

let%html index username = {html|
<html>
<head><title>My Vif Website!</title></head>
<body>
  <p>Hello |html} username {html| !</p>
</body>
</html>
|html} ;;

let index username : Tyxml_html.doc = index [ Html.txt username ] ;;

let index req server _env =
  let open Vif.Response.Syntax in
  match Vif.Request.get jwt req with
  | None ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Some { username } ->
    let* () = Vif.Response.with_tyxml req (index username) in
    Vif.Response.respond `OK
;;
```

What's interesting here is that TyXML will check the HTML content to see if it
complies with standards. Furthermore, if you remove the `<head>` tag, for
example, TyXML will complain that this mandatory tag is missing.

This is not, of course, the only way to deliver content to the client. We can
also respond with JSON as plain text, as we have done since the beginning of
this book. This example essentially shows that it is possible to use `ppx`
within a Vif script.

## Vif & redirections

One last small detail, but one that may be important: users should now be
redirected to the main page if they manage to log in, rather than receiving a
message saying that they are logged in. Vif offers a `redirect_to` function
that allows you to redirect the user to a given `Vif.Uri.t`.

```ocaml
let login req server { secret } =
  let open Vif.Response.Syntax in
  match Vif.Request.of_multipart_form req with
  | Ok ({ username; _ } as v) when login v ->
    let token = Jwto.encode HS512 secret [ "username", username ] in
    let token = Result.get_ok token in
    let str = "Authenticated!\n" in
    let* () = Vif.Cookie.set ~name:"my-token" server req token in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.redirect_to req Vif.Uri.(rel /?? any)
  | Ok _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Error _ ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let str = Fmt.str "Invalid multipart/form-data\n" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond (`Code 422)
;;
```

We can now test our user space with a single command (since `hurl` handles
redirection and cookies):

```shell
$ curl http://localhost:8080/login -F username=robur -F password=42 -L --cookie-jar cookies.txt
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head><title>My Vif Website!</title></head>
<body>
  <p>Hello robur !</p>
</body>
</html>
```

We're starting to get somewhere. We'll continue using the command line, but we
can now test our website with our web browser! We're producing HTML content,
and all we need to do is log in to [http://localhost:8080/login.html][local] to
be redirected to our main page!

## Next steps

At this point, we can consider that the hardest part is done. However, we would
surely like to go further and, in particular, communicate with a database
rather than our simple `verify` function. In the next chapter, we will see how
to extend our user space so that we can use a persistent database to which our
users can register.

[conan]: https://github.com/mirage/conan
[tyxml]: https://github.com/ocsigen/tyxml
[multipart_form]: https://github.com/dinosaure/multipart_form
[mime-type]: https://en.wikipedia.org/wiki/Media_type
[local]: http://localhost:8080/login.html
