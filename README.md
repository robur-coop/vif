# νιϝ, a small framework for building a web server from an OCaml script

(nu)(iota)(digamma)

**disclaimer**: Please note that this is an experimental project. It's also an
opportunity to build something that can be satisfying for web development.
However, we do not recommend using this project in production.

Vif is a small program that runs an OCaml script and launches a Web server from
it. The main idea is to be able to set up a typed Web server as quickly as
possible (note that we use [hurl][hurl], an HTTP client in OCaml)
```shell
$ opam pin add -y https://github.com/robur-coop/vif
$ opam pin add -y https://github.com/robur-coop/hurl
$ opam install vif hurl
$ cat >main.ml <<EOF
#require "vif" ;;

open Vif ;;

let default req server () =
  let field = "content-type" in
  let* () = Response.add ~field "text/html; charset=utf-8" in
  let* () = Response.with_string req "Hello World!" in
  Response.respond `OK
;;

let routes =
  let open Vif.U in
  let open Vif.R in
  let open Vif.T in
  [ get (rel /?? nil) --> default ]

let () =
  Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
EOF
$ vif --pid vif.pid main.ml &
$ hurl http://localhost:8080/
HTTP/1.1 200 OK

connection: close
content-length: 12
content-type: text/html

Hello World!

$ kill -SIGINT $(cat vid.pid)
```

### Examples

The [examples][./examples] folder contains several examples of the use of `vif`.
It shows the management of more complex requests (json, multipart-form, etc.) as
well as the use of an SQL database with [caqti][caqti].

[hurl]: https://github.com/robur-coop/hurl
[caqti]: https://github.com/paurkedal/ocaml-caqti/
