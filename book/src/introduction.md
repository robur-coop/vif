# Vif, a simple webserver in OCaml

Vif is a small program that allows you to initiate a web server ([http/1.1][h1]
& [h2][h2]) from an OCaml script:
```shell
$ opam install vif
$ cat >main.ml <<EOF
#require "vif" ;;
open Vif ;;

let default req _server () =
  let str = "Hello World!\n" in
  let* () = Response.with_string req str in
  Response.respond `OK
;;

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [ get (rel /?? nil) --> default ]
;;

let () = Miou_unix.run @@ fun () ->
  Vif.run routes ()
;;
EOF
$ vif main.ml --pid vif.pid &
[1] 1337
$ curl http://localhost:8080/
Hello World!
$ kill -SIGINT $(cat vif.pid)
```

The aim is to have a small web server for cheap and to be able to iterate on it
quickly without necessarily having a workflow that involves compiling and
executing an OCaml project. The OCaml script is executed by `vif` just as it
could be executed by `ocaml` and it describes what your webserver should do.

In this short tutorial, we will see how to use Vif to implement more or less
complex web services.

## Vif as a library

Of course, vif is also a library. You don't need to write an OCaml script to
get a web server. You can also develop your own application and compile it with
Vif to get an executable that will be your web server.

However, in terms of iterations, the loop _ocaml script → web server test_ is
faster than _ocaml program → compilation → web server test_.

## How to install Vif

Vif is available on opam. You can install it using the command:
```shell
$ opam install vif
```

You can also obtain the development version of Vif via:
```shell
$ opam pin add https://git.robur.coop/robur/vif.git
```

The Vif project is hosted on our server [git.robur.coop][git.robur.coop]. You
can also access a mirror on [GitHub](https://github.com/robur-coop/vif). Issues
and pull requests can be proposed on both repositories (pull requests will be
merged into our cooperative's repository).

[h1]: https://github.com/robur-coop/ocaml-h1
[h2]: https://github.com/anmonteiro/ocaml-h2
[git.robur.coop]: https://git.robur.coop
