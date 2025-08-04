# My first Vif application

To understand Vif, we will iterate several times on a small project whose goal
is to implement a website with a user area. This goal will allow us to see the
main features of Vif. To do this, we will also use `hurl`, an HTTP client in
OCaml that uses the same libraries as Vif. The latter will allow us to inspect
a whole host of details regarding requests, responses and cookies.

Let's start by installing `vif` and `hurl` using `opam`. Next, we will create a
folder and work in it to develop our brand new website.

```shell
$ opam install vif
$ opam install hurl
$ mkdir my-first-vif-application
$ cd my-first-vif-application
```

Vif is a library, but we also offer a small program that allows you to run an
OCaml script and launch an HTTP server. This approach has the advantage of
avoiding the "development, compilation, testing" loop (as is the case with
native languages) and focusing primarily on development and testing.

We will come back to the `vif` tool, the library and the different ways of using
this project in more detail later, but for now let's focus on developing our
website.

## My first webpage with Vif

We are therefore going to create an OCaml script in which we will develop our
website:

```shell
$ cat >main.ml <<EOF
#require "vif" ;;

let () = Miou_unix.run @@ fun () -> Vif.run [] () ;;
EOF
$ vif --pid vif.pid main.ml &
$ hurl http://localhost:8080/
HTTP/1.1 404 Not Found

connection: close
content-length: 120
content-length: 120
content-type: text/plain; charset=utf-8

Unspecified destination / (GET):
user-agent: hurl/0.0.1
host: localhost
connection: close
content-length: 0
$ kill -SIGINT $(cat vif.pid)
```

Here, we write a new file called `main.ml`, which will simply launch the
[Miou][miou] scheduler and the Vif server. We then run this script using `vif`
and make an HTTP request using `hurl` to obtain a response. Finally, we `kill`
the Vif server.

The Vif server responded with a 404 (_not found_) response because no routes are
defined, and it also describes the request it just received. We will therefore
add a route for our website.

```ocaml
#require "vif" ;;

let index req _server () =
  let open Vif.Response.Syntax in
  let str = "Hello World!\n" in
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req str in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [ get (rel /?? any) --> index ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes () ;;
```

```shell
$ vif --pid vif.pid main.ml &
$ hurl https://localhost:8080/
HTTP/1.1 200 OK

connection: close
content-length: 13
content-type: text/plain; charset=utf-8

Hello World!
$ kill -SIGINT $(cat vif.pid)
```

And here is our first page with Vif! We have added a new `index` function that
will process the request and provide a response. In this response, we will
write `"Hello World!"` and send it with the code 200 (`` `OK ``). We can see
that this is indeed what the server responds when we use `hurl`.

This `index` function will be associated with a route `get (rel /?? any)`. This
route allows us to filter requests and specify that the `index` function will
only process GET requests with the path `"/"`.

## Routes

The principle behind routes is fairly simple to understand: it allows you to
associate (`-->`) a path on your website with a function that will process the
request and respond. What Vif brings to the table is the ability to _type_
routes. Another frequently requested route feature is the ability to specify
_holes_ in the path, which would be values provided by the user.

For example, we would like a route in which the user could specify the name:

```ocaml
#require "vif" ;;

let hello req (name : string) _server () =
  let open Vif.Response.Syntax in
  let str = Fmt.str "Hello %S!\n" name in
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req str in
  Vif.Response.respond `OK
;;

let user req (uid : int) _server () =
  let open Vif.Response.Syntax in
  let str = Fmt.str "User %d!\n" uid in
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req str in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [ get (rel / "hello" /% string `Path /?? any) --> hello
  ; get (rel / "user" /% int /?? any) --> user ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes () ;;
```

```shell
$ hurl http://localhost:8080/hello/robur -p b
Hello "robur"!
$ hurl http://localhost:8080/user/42 -p b
User 42!
$ hurl http://localhost:8080/user/robur -p b
Unspecified destination /user/robur (GET):
...
```

As we can see, it is possible to define "holes" in our routes with Vif and
obtain their value in the functions associated with these routes. It is also
possible to _type_ these holes so that you only process a certain type of
information, such as an integer (as is the case for our second `user` function).
Vif will then not only recognise integers, but also transform the value into a
real OCaml integer that you can manipulate.

It is possible to define more complex "holes" that must match a regular
expression. Here is an example where we would like to recognise certain fruits
in our route.

```ocaml
#require "vif" ;;

type fruit =
  | Apple
  | Orange
  | Banana
;;

let pp ppf = function
  | Apple -> Fmt.string ppf "Apple"
  | Orange -> Fmt.string ppf "Orange"
  | Banana -> Fmt.string ppf "Banana"
;;

let fruit =
  let v = Tyre.regex Re.(alt [ str "apple"; str "orange"; str "banana" ]) in
  let inj = function
    | "apple" -> Apple
    | "orange" -> Orange
    | "banana" -> Banana
    | _ -> assert false in
  let prj = function
    | Apple -> "apple"
    | Orange -> "orange"
    | Banana -> "banana" in
  Vif.Uri.conv inj prj v
;;

let like req (fruit : fruit) _server () =
  let open Vif.Response.Syntax in
  let str = Fmt.str "I like %a!\n" pp fruit in
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req str in
  Vif.Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [ get (rel /% fruit /?? any) --> like ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes () ;;
```

```shell
$ hurl http://localhost:8080/orange -p b
I like Orange!
$ hurl http://localhost:8080/cherries -p b
Unspecified destination /cherries (GET):
...
```

Here we describe how to recognise certain fruits using a regular expression
(and thanks to the [`re`][re] library). Next, we describe how to convert
recognised strings into OCaml values (`inj` ). Finally, we use this new value in
our route in order to recognise only the specified fruits.

## Next steps

At this point, you should have a basic understanding of how routes work with
Vif. The next step is to create our user space. This essentially consists of
two routes:
- a route for submitting user credentials to the server
- a route reserved for logged-in users

First, we will focus on the server-side mechanics of credential verification.
These credentials will initially be submitted using JSON.

Next, we will introduce the concept of cookies and middleware, which will allow
us to create our second route (accessible only to logged-in users).

Finally, we will enhance this foundation with other features offered by Vif.

[miou]: https://github.com/robur-coop/miou
[re]: https://github.com/ocaml/re
